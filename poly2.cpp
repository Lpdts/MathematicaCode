#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 2048   // 数组strLine 的长度
#define VAR_LEN 8  // 变量的长度

void delBlank(char* a) {
    int i = 0, j = 0;
    while(*(a + i) != '\0') {
        if(*(a + i) != ' ') {
            *(a + j) = *(a + i);
            j++;
            i++;
        }else {
            i++;
        }
    }
    *(a + j) = '\0';
}

void delVar(char* poly, char* var) {
    int i = 0, left = 0, j = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '=') {
            for(j = left; j <= i; j++) {
                // 如果匹配到var，则删除var所在的项
                if(*(poly + j) == *var) {
                    if(*(poly + j + 1) == *(var + 1) && *(poly + j + 2) == *(var + 2) && *(poly + j + 3) == *(var + 3)
                       && *(poly + j + 4) == *(var + 4) && *(poly + j + 5) == *(var + 5)) {
                        int k = j;
                        while(*(poly + k) != '=' && *(poly + k) != '+' && *(poly + k) != '-') {
                            *(poly + k) = ' ';
                            k++;
                        }
                        k = j;
                        while(k != left - 1) {
                            if(*(poly + k) == '+' || *(poly + k) == '-') {
                                *(poly + k) = ' ';
                                break;
                            }
                            *(poly + k) = ' ';
                            k--;
                        }
                    }
                }
            }
            left = j;
        }
        i++;
    }
}

// 找到形如 x^n = 0 的项
int findVar(char* poly, char* var) {
    int i = 0, left = 0, j = 0;
    int numOfBracket = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '[') {
            numOfBracket++;
            left = i;   // 记录左中括号的位置
        }
        if(*(poly + i) == '=') {
            if(numOfBracket == 1) {
                for(j = 0; j <= 5; j++) {
                    *(var + j) = *(poly + left + j);
                }
                *(var + j) = '\0';
                return 1;
            }
            numOfBracket = 0;
        }
        i++;
    }
    // 不存在，返回0
    return 0;
}

// 找到形如 xy = 0 的项
int findVar2(char* poly, char * var) {
    int i = 0, left = 0;
    int numOfPlusSub = 0;

    while(*(poly + i) != '\0') {
        if((*(poly + i) == '+' || *(poly + i) == '-') && i != 0 && *(poly + i - 1) != '=') {
            numOfPlusSub++;
        }
        if(*(poly + i) == '=') {
            if(numOfPlusSub == 0) {
                //todo
                return 1;
            }
        }
        i++;
    }
    return 0;
}

int main()
{
    char polyFileName[] = "C:\\Users\\zhangshutao\\Desktop\\MathematicaCode\\poly.txt";

    FILE* pFile;
    char strLine[LEN];

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("无法打开文件");
        return -1;
    }

    fgets(strLine, LEN, pFile);
    //printf("%s\n", strLine);

    int len = strlen(strLine);   // 字符串strLine 的长度

    // 删除字符串的空格
    delBlank(strLine);

    //printf("%s\n", strLine);

    char var[VAR_LEN];  // 变量数组

    while(findVar(strLine, var)) {
        delVar(strLine, var);
        delBlank(strLine);
        printf("%s]=0\n", var);
    }
    printf("%s\n", strLine);
}
