module Pie where
data Term = Name String
          | Var  String
          | Func String [Term]
          deriving (Ord, Show, Eq)

data Process =
    Inactive
  | Parallel  Process Process
  | Replicate Process
  | Choice    Process Process
  | New       String  Process
  | IfThen    Term Term Process Process
  | Print     Term Process
  | UserInput String Process
  deriving (Show, Eq)

data Action = PrintAction Term | InputAction String | Tau
  deriving (Show, Eq)

-- Substitution function: replaces variables with values
substitute :: String -> Term -> Process -> Process
substitute x val (IfThen t1 t2 p q) = IfThen (subTerm t1) (subTerm t2) (substitute x val p) (substitute x val q)
  where subTerm (Var v) | v == x = val
        subTerm t = t
substitute x val (Print t p) = Print (subTerm t) (substitute x val p)
  where subTerm (Var v) | v == x = val
        subTerm t = t
substitute x val (UserInput y p) = UserInput y (substitute x val p)
substitute x val (Parallel p q) = Parallel (substitute x val p) (substitute x val q)
substitute x val (Choice p q) = Choice (substitute x val p) (substitute x val q)
substitute x val (New y p) = New y (if x == y then p else substitute x val p)
substitute _ _ p = p  -- Base case: no change

-- Small-step semantics: One step of execution
step :: Process -> IO (Maybe (Action, Process))
step Inactive = return Nothing  -- No transition for Inactive

-- Apply the PAR rule: If `p` steps, then `p | q` steps
step (Parallel p q) = do
  resP <- step p
  case resP of
    Just (action, p') -> return $ Just (action, Parallel p' q)
    Nothing -> do
      resQ <- step q
      case resQ of
        Just (action, q') -> return $ Just (action, Parallel p q')
        Nothing -> return Nothing

-- Choice rule: Take the first process in non-deterministic choice
step (Choice p _) = return $ Just (Tau, p)  -- Choose `p` (non-deterministic)

-- If-Then-Else rule
step (IfThen t1 t2 p q)
  | t1 == t2  = return $ Just (Tau, p)  -- Condition is true, take first branch
  | otherwise = return $ Just (Tau, q)  -- Condition is false, take second branch

-- Print action
step (Print t p) = return $ Just (PrintAction t, p)

-- User input: Read from terminal, then substitute input into process
step (UserInput x p) = do
  putStr (x ++ " = ")
  input <- getLine
  let term = Name input  -- Treat input as a "name" for now
  return $ Just (InputAction x, substitute x term p)

-- Replication: `!P` behaves like `P | !P`
step (Replicate p) = return $ Just (Tau, Parallel p (Replicate p))

-- RES rule: `New x P` steps if `P` steps, but ensures `x` is not leaked
step (New x p) = do
  res <- step p
  case res of
    Just (action, p') -> return $ Just (action, New x p')  -- Ensure `x` is not leaked
    Nothing -> return Nothing

-- Running the interpreter
run :: Process -> IO ()
run Inactive = return ()
run p = do
  res <- step p
  case res of
    Just (PrintAction t, p') -> print t >> run p'
    Just (InputAction x, p') -> run p'  -- User input already handled
    Just (_, p') -> run p'  -- Skip internal steps (Tau)
    Nothing -> return ()  -- No more steps
