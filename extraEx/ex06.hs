import Data.List(nub)

main ::IO()
main = do
    print(hardestSubject rs)   
    



--Defining our types
type Student = String
type Subject = String
type Note = Double

type Record = (Student, Subject, Note)

--Some records
r1,r2,r3,r4,r5,r6 :: Record
r1 = ("Ivan",   "Algebra",  5.5)
r2 = ("Georgi", "Algebra",  6.0)
r3 = ("Atanas", "Algebra",  6.25)
r4 = ("Atanas", "Algebra",  6.0)
r5 = ("Ivan",   "DIS", 5.0)
r6 = ("Georgi", "Geometry", 5.0)
r7 = ("Atanas", "DSTR", 3.0)

--Making a list of all records
rs :: [Record]
rs = [r1,r2,r3,r4,r5,r6,r7]

hardestSubject :: [Record] -> Subject
hardestSubject [] = error "There are no records!"
hardestSubject recList@(record1@(stName, suName, suGrade) : xs) = smallestGrade(gradesPerSubject recList)
    where
        subjectNames = nub [name | (_, name, _) <- recList]

        gradesPerSubject :: [Record] -> [(Subject,Double)]
        gradesPerSubject subs = [(name, avg [grade2| (_,name2, grade2) <- subs, name2 == name]) | name <- subjectNames]                         
        
        smallestGrade::[(Subject,Double)] -> Subject
        smallestGrade grades = hasMinGrade grades
            where
                subjectMinGrade = minElem [grade | (_,grade) <- grades]

                hasMinGrade::[(Subject,Double)] -> Subject
                hasMinGrade ((subName, subGrade):subList)
                    | subGrade == subjectMinGrade = subName
                    | otherwise = hasMinGrade subList 
                    


avg :: (Fractional a) => [a] -> a
avg []   = error "Cannot take average of empty list"
avg nums = let (sum,count) = foldr (\e (s,c) -> (s+e,c+1)) (0,0)  nums
            in sum / count

minElem::[Double]->Double
minElem [] = 0
minElem [x] = x
minElem (x:y:xs) 
 |x > y = minElem (y:xs)
 |x <= y = minElem (x:xs)
 |x == y = minElem (x:xs)