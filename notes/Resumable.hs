module Notes.Resumable where

type Handler eff err r =
  ∀ f x r0 . Functor f => eff (Sem r0) x -> Sem (Tactics f (Sem r0) (Resumable err eff : Stop err : r) : Stop err : r) (f x)

liftWeaving ::
  ∀ eff err r0 r a f fOuter r0Outer res resOuter .
  Functor f =>
  Functor fOuter =>
  Handler eff err r ->
  eff (Sem r0) a ->
  f () ->
  fOuter () ->
  (∀ x . f (Sem r0 x) -> Sem r0Outer (f x)) ->
  (∀ x . fOuter (Sem r0Outer x) -> Sem (Resumable err eff : r) (fOuter x)) ->
  (f a -> res) ->
  (fOuter (Either err res) -> resOuter) ->
  (∀ x . f x -> Maybe x) ->
  (∀ x . fOuter x -> Maybe x) ->
  Sem r resOuter
liftWeaving handler e s sOuter dist distOuter ex exOuter ins insOuter =
  exFinal <$> runStop (runTactics sFinal distBasic insFinal recurse (handler e))
  where
    exFinal :: Either err (Compose fOuter f a) -> resOuter
    exFinal = exOuter . \case
      Right (getCompose -> a) -> Right . ex <$> a
      Left err -> Left err <$ sOuter
    sFinal :: Compose fOuter f ()
    sFinal =
      Compose (s <$ sOuter)
    insFinal :: (Compose fOuter f) x -> Maybe x
    insFinal =
      join . fmap ins . insOuter . getCompose
    recurse :: ∀ x . Compose fOuter f (Sem r0 x) -> Sem (Stop err : r) (Compose fOuter f x)
    recurse fx =
      raise (interpretResumableH handler (distRecurse fx))
    distRecurse :: ∀ x . (Compose fOuter f) (Sem r0 x) -> Sem (Resumable err eff : r) ((Compose fOuter f) x)
    distRecurse (getCompose -> fsem) =
      Compose <$> distOuter (dist <$> fsem)
    distBasic :: ∀ x . (Compose fOuter f) (Sem r0 x) -> Sem (Resumable err eff : Stop err : r) ((Compose fOuter f) x)
    distBasic (getCompose -> fsem) =
      raiseUnder (Compose <$> distOuter (dist <$> fsem))

interpretResumableH ::
  ∀ (eff :: Effect) (err :: *) (r :: EffectRow) a .
  Handler eff err r ->
  Sem (Resumable err eff : r) a ->
  Sem r a
interpretResumableH handler (Sem m) =
  Sem \ k -> m \ u ->
    case decomp u of
      Right (Weaving (Resumable (Weaving e s dist ex ins)) sOuter distOuter exOuter insOuter) ->
        usingSem k (liftWeaving handler e s sOuter dist distOuter ex exOuter ins insOuter)
      Left x ->
        k $ hoist (interpretResumableH handler) x
