module Bags where
    import Data.List
    -- datatype for a bag
    type Bag a = [(a,Int)] 

    {-|
       bagInsert takes an object and a empty/non-empty bag as 
       parameters and adds the object to the bag
    -}
    bagInsert :: Eq a => a -> Bag a -> Bag a
    bagInsert o [] = [(o,1)]
    bagInsert o ((o2,i):xs)
        | o2 == o = (o,i+1):xs
        | otherwise = (o2,i) : bagInsert o xs
        
    {-|
       listToBag takes a list as a parameter and returns - through using an 
       auxiliary function a bag with the same elements as the list
    -}
    listToBag :: Eq a => [a] -> Bag a
    listToBag lis = listToBagA lis []
    
    {-|
       listToBagA - Auxiliary function for listToBag
       takes the list from listToBag and adds it to a bag
    -}
    listToBagA :: Eq a => [a] -> Bag a -> Bag a
    listToBagA [] bag = bag
    listToBagA (h:t) bag = (bagInsert h (listToBagA t bag)) -- uses bagInsert

    {-|
       bagIntersection takes two bags as parameters and 
       through an auxiliary function it returns all the common elements in both bags
    -}
    bagIntersection :: Eq a => Bag a -> Bag a -> Bag a
    bagIntersection [] [] = []
    bagIntersection [] _ = []
    bagIntersection _ [] = []
    bagIntersection bag1 bag2 = (bagIntersection xs bag2) ++ (itemIntersection [(o1,i1)] bag2)
        where ((o1,i1):xs) = bag1
              ((o2,i2):ys) = bag2
    {-|
       itemIntersection - adds one item at a time 
       to the intersection of two bags
    -}
    itemIntersection :: Eq a => Bag a -> Bag a -> Bag a
    itemIntersection _ [] = []
    itemIntersection bag1 bag2 
        | o1 == o2 = [(o1,(min i1 i2))]
        | otherwise = itemIntersection [(o1,i1)] ys
        where ((o1,i1):xs) = bag1
              ((o2,i2):ys) = bag2

    {-| 
    bagEqual takes two bags and returns them to it's auxiliary 
    function already sorted by their first element using the Data.List function 
    -}
    bagEqual :: Ord a => Bag a -> Bag a -> Bool
    bagEqual bag1 bag2 = (bagEqualA (sort bag1) (sort bag2))  
    
    {-| 
    bagEqualA takes two parameters from bagEqual and compares each head together 
    then recursively calls to compare the tails, if all true then the bags are equal
    -}
    bagEqualA :: Ord a => Bag a -> Bag a -> Bool
    bagEqualA [] [] = True
    bagEqualA _ [] = False
    bagEqualA [] _ = False
    bagEqualA (h:t) (h2:t2)
        | h == h2 = bagEqualA t t2
        | otherwise = False
                           
    {-| 
    bagInsertX is a version of bagInsert used to input an integer value (occurence of said item)
    along with the object and return a bag with the object and its corresponding integer value
    -}                        
    bagInsertX :: Eq a => a -> Int -> Bag a -> Bag a
    bagInsertX o x [] = [(o,x)]
    bagInsertX o x ((o2,i):hs)
        | o2 == o = (o,i+x):hs
        | otherwise = (o2,i) : bagInsertX o x hs
             
    {-| 
    bagsum is a function that takes two bags as its input and recursively 
    adds a new item to a bag using bagInsertX
    -}         
    bagSum :: Eq a => Bag a -> Bag a -> Bag a
    bagSum [] [] = []
    bagSum bag [] = bag
    bagSum [] bag = bag
    bagSum bag1 bag2 = bagSum xs (bagInsertX o1 i1 bag2)            
        where ((o1,i1):xs) = bag1
              ((o2,i2):ys) = bag2
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    
    