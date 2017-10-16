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
    int i = 0, left = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '=') {

        }
    }
}

int findVar(char* poly, char* var) {
    int i = 0, left = 0;
    int numOfBracket = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '[') {
            numOfBracket++;
            left = i;   // 记录左中括号的位置
        }
        if(*(poly + i) == '=') {
            if(numOfBracket == 1) {
                for(int j = 0; j <= 5; j++) {
                    *(var + j) = *(poly + left + j);
                }
                *(var + j) = '\0';
                return 1;
            }
        }
        i++;
    }
    // 不存在，返回0
    return 0;
}

int main()
{
    char polyFileName[] = "";

    FILE* pFile;
    char strLine[LEN];

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("无法打开文件");
        return -1;
    }

    fgets(strLine, LEN, pFile);
    int len = strlen(strLine);   // 字符串strLine 的长度

    // 删除字符串的空格
    delBlank(strLine);

    char var[VAR_LEN];  // 变量数组

    while(findVar(strLine, var)) {
        delVar(strLine, var);
    }
}
