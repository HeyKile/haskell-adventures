
-- print every string in a given list
printList [] = return ()
printList (x:xs) = do
    print x
    print xs
    