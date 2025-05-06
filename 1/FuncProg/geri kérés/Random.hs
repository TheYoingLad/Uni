module What where

test = [83, 162, 83, 46, 36, 162, 83, 83, 175, 162, 64, 181, 46, 36, 46, 181, 64, 162, 83, 162, 180, 150, 162]
test2 = map (\x -> mod (x^7) 187) test
test3 = [' ', 'A', 'E', 'E', 'G', 'K', 'R', 'S', 'T', 'Z', '?']
test4 = map (\x -> test3!!x) test2