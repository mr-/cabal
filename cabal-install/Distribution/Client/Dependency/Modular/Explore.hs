module Distribution.Client.Dependency.Modular.Explore
  (runTreePtrLog, transformLog, exploreTreePtrLog, backjump, ptrToAssignment)
where

import Control.Applicative as A
import Data.Foldable
import Data.List as L
import Data.Map as M
import Data.Set as S
import Data.Maybe (fromJust)

import Distribution.Client.Dependency.Modular.Assignment
import Distribution.Client.Dependency.Modular.Dependency
import Distribution.Client.Dependency.Modular.Log
import Distribution.Client.Dependency.Modular.Message
import Distribution.Client.Dependency.Modular.Package
import Distribution.Client.Dependency.Modular.PSQ as P
import Distribution.Client.Dependency.Modular.Tree
import Distribution.Client.Dependency.Modular.TreeZipper



-- | Backjumping.
--
-- A tree traversal that tries to propagate conflict sets
-- up the tree from the leaves, and thereby cut branches.
-- All the tricky things are done in the function 'combine'.
backjump :: Tree a -> Tree (Maybe (ConflictSet QPN))
backjump = snd . cata go
  where
    go (FailF c fr) = (Just c, Fail c fr)
    go (DoneF rdm ) = (Nothing, Done rdm)
    go (PChoiceF qpn _     ts) = (c, PChoice qpn c     (P.fromList ts'))
      where
        ~(c, ts') = combine (P qpn) (P.toList ts) S.empty
    go (FChoiceF qfn _ b m ts) = (c, FChoice qfn c b m (P.fromList ts'))
      where
        ~(c, ts') = combine (F qfn) (P.toList ts) S.empty
    go (SChoiceF qsn _ b   ts) = (c, SChoice qsn c b   (P.fromList ts'))
      where
        ~(c, ts') = combine (S qsn) (P.toList ts) S.empty
    go (GoalChoiceF        ts) = (c, GoalChoice        (P.fromList ts'))
      where
        ~(cs, ts') = unzip $ L.map (\ (k, (x, v)) -> (x, (k, v))) $ P.toList ts
        c          = case cs of []    -> Nothing
                                d : _ -> d

-- | The 'combine' function is at the heart of backjumping. It takes
-- the variable we're currently considering, and a list of children
-- annotated with their respective conflict sets, and an accumulator
-- for the result conflict set. It returns a combined conflict set
-- for the parent node, and a (potentially shortened) list of children
-- with the annotations removed.
--
-- It is *essential* that we produce the results as early as possible.
-- In particular, we have to produce the list of children prior to
-- traversing the entire list -- otherwise we lose the desired behaviour
-- of being able to traverse the tree from left to right incrementally.
--
-- We can shorten the list of children if we find an individual conflict
-- set that does not contain the current variable. In this case, we can
-- just lift the conflict set to the current level, because the current
-- level cannot possibly have contributed to this conflict, so no other
-- choice at the current level would avoid the conflict.
--
-- If any of the children might contain a successful solution
-- (indicated by Nothing), then Nothing will be the combined
-- conflict set. If all children contain conflict sets, we can
-- take the union as the combined conflict set.
combine :: Var QPN -> [(a, (Maybe (ConflictSet QPN), b))] ->
           ConflictSet QPN -> (Maybe (ConflictSet QPN), [(a, b)])
combine _   []                      c = (Just c, [])
combine var ((k, (     d, v)) : xs) c = (\ ~(e, ys) -> (e, (k, v) : ys)) $
                                        case d of
                                          Just e | not (var `S.member` e) -> (Just e, [])
                                                 | otherwise              -> combine var xs (e `S.union` c)
                                          Nothing                         -> (Nothing, snd $ combine var xs S.empty)


-- | Add in information about pruned trees.
--
-- TODO: This isn't quite optimal, because we do not merely report the shape of the
-- tree, but rather make assumptions about where that shape originated from. It'd be
-- better if the pruning itself would leave information that we could pick up at this
-- point.
backjumpInfo :: Maybe (ConflictSet QPN) -> Log Message a -> Log Message a
backjumpInfo c m = m <|> case c of -- important to produce 'm' before matching on 'c'!
                           Nothing -> A.empty
                           Just cs -> failWith (Failure cs Backjump)



-- given a pointer, calculate the Assignment up to this point.
-- TODO: This does not really need to be using cata. The Trail should be
-- enough.
ptrToAssignment :: Pointer a -> Assignment
ptrToAssignment ptr = --transformLog $ exploreTreePtrLog ptr (backjump $ toTree ptr)
 mkAssignment (toTree $ focusRoot ptr) (A M.empty M.empty M.empty, oneWayTrail)
  where
    oneWayTrail = wrongToOne $ pathToTrail $ toPath ptr -- the trail to the Done-Node

    mkAssignment :: Tree a -> (Assignment, OneWayTrail) -> Assignment
    mkAssignment = cata go
      where
        go :: TreeF t ((Assignment, [ChildType]) -> Assignment) -> (Assignment, [ChildType]) -> Assignment
        go _              (a, [])                           = a

        go (PChoiceF qpn _     ts) (A pa fa sa, (CTP k):xs) = r (A (M.insert qpn k pa) fa sa, xs)
          where r = fromJust $ P.lookup k ts

        go (FChoiceF qfn _ _ _ ts) (A pa fa sa, (CTF k):xs) = r (A pa (M.insert qfn k fa) sa, xs)
          where r = fromJust $ P.lookup k ts

        go (SChoiceF qsn _ _   ts) (A pa fa sa, (CTS k):xs) = r (A pa fa (M.insert qsn k sa), xs)
          where r = fromJust $ P.lookup k ts

        go (GoalChoiceF        ts) (a, (CTOG k):xs)         = v (a, xs )
          where v = fromJust $ P.lookup k ts

        go _                       _                         = error "Internal error in donePtrToLog"
                                                 -- This catches cases like (GoalChoiceF...) (a (CTP k)..)
                                                 -- where childtype and nodetype don't match
                                                 -- ugly? Maybe. Could have been easy with dependent types.


-- | Version of 'explore' that returns a 'Log'.
explorePtrLog :: Tree (Maybe (ConflictSet QPN)) -> Pointer a -> Log Message (Pointer a)
explorePtrLog tree pointer = (fromJust . (flip walk) pointer . wrongToOne) <$> (worker tree [])
  where
  worker :: Tree (Maybe (ConflictSet QPN)) -> WrongWayTrail -> Log Message WrongWayTrail
  worker = cata go
    where
      go (FailF c fr)          _                          = failWith (Failure c fr)
      go (DoneF _)               treePtr                  = succeedWith Success treePtr
      go (PChoiceF qpn c     ts) treePtr                  =
        backjumpInfo c $
        asum $                                      -- try children in order,
        P.mapWithKey                                -- when descending ...
          (\ k r -> tryWith (TryP (PI qpn k)) $     -- log and ...
                      r ((CTP k):treePtr))  -- record the pkg choice
        ts
      go (FChoiceF qfn c _ _ ts) treePtr    =
        backjumpInfo c $
        asum $                                      -- try children in order,
        P.mapWithKey                                -- when descending ...
          (\ k r -> tryWith (TryF qfn k) $          -- log and ...
                      r ((CTF k):treePtr)) -- record the pkg choice
        ts
      go (SChoiceF qsn c _   ts) treePtr    =
        backjumpInfo c $
        asum $                                      -- try children in order,
        P.mapWithKey                                -- when descending ...
          (\ k r -> tryWith (TryS qsn k) $          -- log and ...
                      r ((CTS k):treePtr)) -- record the pkg choice
        ts
      go (GoalChoiceF        ts) treePtr             =
        casePSQ ts
          (failWith (Failure S.empty EmptyGoalChoice))   -- empty goal choice is an internal error
          (\ k v _xs -> continueWith (Next (close k))
              (v ( (CTOG k):treePtr )))     -- commit to the first goal choice


-- | Interface. -- This finds a path in offsetPtr while traversing conflictTree.
                -- It is important that conflictTree is a subtree of offsetPtr's tree
                -- Or else it fails with a fromJust error :->
                -- This should definitely be handled differently..
                -- Maybe: data Subtree = Foo (Pointer a) (Tree b)
                -- Or something..
                --
                -- TODO: That may work if I only provide "smart"
                -- constructors that ensure that property.
                -- However, the tree comes from backjump..
                -- Need to think more.
                --
                -- Note that this does not backtrack below the pointer,
                -- so it is save to give it that "offsetAssignment"
exploreTreePtrLog :: Pointer a -> Tree (Maybe (ConflictSet QPN)) -> Log Message (Pointer a)
exploreTreePtrLog offsetPtr conflictTree = explorePtrLog conflictTree offsetPtr


-- | Interface. -- This is to consume the Log and give either a Done-Ptr or an error.
-- Where, oh where should you go? Here is Ok, I guess.. Now this module knows how to make Log Message (...) and it also knows
-- how to get information out of it.
-- Maybe we would also like the Assignment?
runTreePtrLog :: Log Message a -> Either String a
runTreePtrLog l = case runLog l of
                    (ms, Nothing) -> Left $ unlines $ showMessages (const True) True ms
                    (_, Just x)   -> Right x



transformLog :: Log Message (Pointer a) -> Log Message (Assignment, RevDepMap)
transformLog mLog = (\x -> (ptrToAssignment x, fromDone x)) <$> mLog
  where
    fromDone :: Pointer a -> RevDepMap
    fromDone (Pointer _ (Done rdm)) = rdm
    fromDone (Pointer _ _)          = error "Uhoh.. Internal error in exploreTreeLog. Have you tried turning it off and on again?"
