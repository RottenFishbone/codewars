import Text.Regex.TDFA

{- 
    This regex is the result of building a DFA for division of 7 in binary strings
    e.g. [visualize at: https://mermaid.live]
    ```mermaid
    stateDiagram-v2
    direction LR
        0-->1:0
        0-->1:1
        1-->2:0
        1-->3:1
        2-->4:0
        2-->5:1
        3-->6:0
        3-->0:1
        4-->1:0
        4-->2:1
        5-->3:0
        5-->4:1
        6-->5:0
        6-->6:1
    ```

    Using the DFA I converted to a regular expression, resulting in this output
-}
solution :: String
solution = "^(0|1((0|1(01*0{2})*01*01{2})(((0|1{2})|10(01*0{2})*01*01)1)*((0|1{2})0|10(01*0{2})*(01*010|10*1))|1(01*0{2})*(01*010|10*1))*((0|1(01*0{2})*01*01{2})(((0|1{2})|10(01*0{2})*01*01)1)*10|1)(01*0{2})*10*)$"


