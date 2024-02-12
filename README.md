# Business Rules Extraction Benchmark Dataset
This repository contains a curated collection of programs and corresponding business rules that serves as a comprehensive resource for evaluating and advancing the Business Rules Extraction processes. The dataset encompasses a diverse range of programs, including both open-source and those generated using cutting-edge generative AI tools. We have also shared the Business Rules extracted by our tool corresponding to the programs.

## Folder Content
All the folders present in the root directory, are the COBOL program names each of which contains 3 files as follows:
* \<program-name\> : COBOL program
* \<program-name\>_GT.md : Contains the golden truth of the Business rules
* \<program-name\>_Rules.md : Contains the rules that we have extracted with the help of our tool.

## Dataset properties
| S. No. | File Name           | Cyclomatic Complexity | Lines of code | No. unique constructs | No. all constructs |
| ------ | ------------------- | --------------------- | ------------- | --------------------- | ------------------ |
| 1      | knuth-shuffle.cbl   | 2                     | 22            | 5                     | 8                  |
| 2      | PRIME.cbl           | 5                     | 26            | 7                     | 12                 |
| 3      | BMI.cbl             | 7                     | 58            | 9                     | 30                 |
| 4      | CARRENT.cbl         | 8                     | 81            | 13                    | 32                 |
| 5      | LUHN.cbl            | 8                     | 61            | 17                    | 267                |
| 6      | AESXGET.cbl         | 10                    | 130           | 14                    | 55                 |
| 7      | didzorchcancelnight.cbl | 10                | 50            | 8                     | 29                 |
| 8      | testgen.cbl         | 11                    | 72            | 8                     | 32                 |
| 9      | worker.cob          | 12                    | 88            | 12                    | 50                 |
| 10     | LOANPYMT.cbl        | 13                    | 150           | 11                    | 90                 |
| 11     | ATM.cbl             | 17                    | 83            | 13                    | 57                 |
| 12     | CALC.cbl            | 17                    | 103           | 10                    | 80                 |
| 13     | FAKERGEN.cob        | 20                    | 314           | 13                    | 62                 |
| 14     | FAKBANK.cbl         | 23                    | 364           | 15                    | 69                 |
| 15     | HEATINDX.cbl        | 26                    | 217           | 13                    | 101                |
| 16     | shop.cbl            | 27                    | 144           | 11                    | 84                 |
| 17     | TICTACTOBOL.cbl     | 28                    | 247           | 17                    | 97                 |
| 18     | POKER.cbl           | 29                    | 101           | 12                    | 58                 |
| 19     | FAKTXID.cob         | 46                    | 558           | 13                    | 105                |
| 20     | eleve.cob           | 48                    | 242           | 19                    | 132                |
| 21     | FAKCOMP.cob         | 51                    | 715           | 16                    | 147                |
| 22     | CWKTCOBX.cbl        | 52                    | 530           | 15                    | 212                |
| 23     | AESMAIN.cbl         | 92                    | 549           | 12                    | 306                |
| 24     | IB4OP01.cob         | 112                   | 683           | 13                    | 327                |
| 25     | CWBWCOBX.cbl        | 52                    | 347           | 14                    | 212                |
| 26     | CTXTA3B.cob         | 15                    | 37            | 7                     | 21                 |
| 27     | FAKERTST.cob        | 16                    | 143           | 14                    | 53                 |
|        | Average             | 28.04                 | 226.48        | 12.26                 | 101.04             |
