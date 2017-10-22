#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 2048   // 数组strLine 的长度
#define VAR_LEN 8  // 变量的长度

typedef struct _POLYS {
    char poly[LEN];
    struct _POLYS *next;
}POLYS;

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

// 从字符串*poly中
void delStr(char *poly, char* str){

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
int findVar2(char* poly, char* item) {
    int i = 0, j = 0, left = 0;  // xy=0项的左侧
    int numOfPlusSub = 0;  // +,-的个数，去掉第一个后个数为零则为xy=0的项

    while(*(poly + i) != '\0') {
        if((*(poly + i) == '+' || *(poly + i) == '-') && i != 0 && *(poly + i - 1) != '=') {
            numOfPlusSub++;
        }
        if(*(poly + i) == '=' && *(poly + i - 1) != '=') { // 去掉==的情况
            if(numOfPlusSub == 0) {
                for(j = 0; left + j < i; j++) {
                    *(item + j) = *(poly + left + j);
                }
                *(item + j) = '\0';
                return 1;
            }else {
                numOfPlusSub = 0;
                left = i + 1;
            }
        }
        i++;
    }
    return 0;
}

int main()
{
    char polyFileName[] = "poly.txt";

    FILE* pFile;
    //char strLine[LEN];
    POLYS polys;
    polys.next = NULL;

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("无法打开文件");
        return -1;
    }

    fgets(polys.poly, LEN, pFile);
    //printf("%s\n", strLine);

    int len = strlen(polys.poly);   // 字符串strLine 的长度

    // 删除字符串的空格
    delBlank(polys.poly);

    printf("%s\n", polys.poly);

    char var[VAR_LEN];  // 变量数组

    while(findVar(polys.poly, var)) {
        delVar(polys.poly, var);
        delBlank(polys.poly);
        printf("%s]=0\n", var);
    }
    printf("%s\n", polys.poly);


    char item[100];
    if(findVar2(polys.poly, item)){
        printf("%s\n", item);
        int i = 0, j = 0;
        while(*(item + i) != '\0') {
            if(*(item + i) == '[') {
                for(j = 0; j <= 5; j++) {
                    *(var + j) = *(item + i + j);
                }
                *(var + j) = '\0';
                printf("%s\n", var);
                POLYS newPolys;
                newPolys.next = NULL;
                strcpy(newPolys.poly, polys.poly);
                delVar(newPolys.poly, var);
                printf("%s\n", newPolys.poly);
            }
            i++;
        }
    }


}
