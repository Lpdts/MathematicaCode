#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 2048   // 数组strLine 的长度
#define VAR_LEN 8  // 变量的长度

typedef struct _POLYS {
    char poly[LEN];
    struct _POLYS *next;
}* POLYS;

//删除字符串*a中的空格
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

// 从字符串*poly中 删除字符串*str
void delStr(char *poly, char* str){
    delBlank(poly);   // 删除空格
    for(int i = 0; *(poly + i) != '\0'; i++) {
        if(*(poly + i) == *str) {
            int f = 1;
            for(int j = 1; *(str + j) != '\0'; j++) {
                if(*(poly + i + j) != *(str + j)) {
                    f = 0;
                    break;
                }
            }
            if(f){
                for(int j = 0; *(str + j) != '\0'; j++) {
                    *(poly + i + j) = ' ';
                }
            }
        }
    }
    delBlank(poly);  // 删除空格
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
    delBlank(poly);  // 删除含有var的项后，再删除空格
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

    char var[VAR_LEN];  // 变量数组
    // 删除x^n = 0 的项
    while(findVar(poly, var)) {
        delVar(poly, var);
        printf("%s]=0\n", var);
    }
    printf("%s\n", poly);
    system("pause");

    int i = 0, j = 0, left = 0;  // xy=0项的左侧
    int numOfPlusSub = 0;  // +,-的个数，去掉第一个后个数为零则为xy=0的项
    int value = 0;  // 函数返回值

    while(*(poly + i) != '\0') {
        if((*(poly + i) == '+' || *(poly + i) == '-') && i != 0 && *(poly + i - 1) != '=') {
            numOfPlusSub++;
        }
        if(*(poly + i) == '=' && *(poly + i - 1) != '=') { // 去掉==的情况
            if(numOfPlusSub == 0) {
                /*for(j = 0; left + j < i; j++) {
                    *(item + j) = *(poly + left + j);
                }
                *(item + j) = '\0';
                return 1;*/
                for(int k = 0; left + k <= i; k++) {
                    *(item + j) = *(poly + left + k);
                    j++;
                }
                value = 1;
            }else {
                numOfPlusSub = 0;
            }
            left = i + 1;
        }
        i++;
    }
    *(item + j) = '\0';
    // 去重
    for(int k = 0; *(item + k) != '\0'; k++) {
        if(*(item + k) == '[') {
            for(int t = k + 6; *(item + t) != '\0'; t++) {
                if(*(item + t) == '['){
                    if(*(item + k + 1) == *(item + t + 1) && *(item + k + 2) == *(item + t + 2) &&
                       *(item + k + 3) == *(item + t + 3) && *(item + k + 4) == *(item + t + 4) &&
                       *(item + k + 5) == *(item + t + 5)) {
                        int m;
                        for(m = t; *(item + m) != ']'; m++) {
                            *(item + m) = ' ';
                        }
                        *(item + m) = ' ';
                    }
                }
            }
        }
    }
    delBlank(item);  // 删除空格
    return value;
}

int main()
{
    char polyFileName[] = "poly.txt";

    FILE* pFile;
    //char strLine[LEN];
    POLYS headPolys, tailPolys, curPolys, newPolys;  // 链表首节点,尾节点,当前节点,新的节点
    tailPolys = headPolys = (POLYS)malloc(sizeof(struct _POLYS));
    headPolys->next = NULL;

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("无法打开文件");
        return -1;
    }

    fgets(headPolys->poly, LEN, pFile);
    printf("%s\n", headPolys->poly);

    delStr(headPolys->poly, "Subscript");    //Subscript   Subsuperscript
    delStr(headPolys->poly, "Subsuperscript");
    printf("%s\n", headPolys->poly);

    int len = strlen(headPolys->poly);   // 字符串strLine 的长度

    char var[VAR_LEN];  // 变量数组
    char item[100];
    while(headPolys != NULL){
        if(findVar2(headPolys->poly, item)){
            printf("项: %s\n", item);
            int i = 0, j = 0;
            while(*(item + i) != '\0') {
                if(*(item + i) == '[') {
                    for(j = 0; j <= 5; j++) {
                        *(var + j) = *(item + i + j);
                    }
                    *(var + j) = '\0';
                    printf("%s\n", var);
                    newPolys = (POLYS)malloc(sizeof(struct _POLYS));
                    newPolys->next = NULL;
                    strcpy(newPolys->poly, headPolys->poly);
                    delVar(newPolys->poly, var);
                    printf("%s\n", newPolys->poly);

                    tailPolys->next = newPolys;
                    tailPolys = tailPolys->next;
                }
                i++;
            }

            curPolys = headPolys;
            headPolys = headPolys->next;
            //free(curPolys->next);
            //free(curPolys);
        }
    }
}
