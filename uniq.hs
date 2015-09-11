-- prunes a list down to its unique elements
uniq :: (Eq a) => [a] -> [a]
uniq []      = []
uniq (el:list) = (if el `elem` recurse then recurse else el:recurse)
    where recurse = uniq list
