#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 102400  //12650 // 数组strLine 的长度
#define VAR_LEN 16  // 变量的长度

typedef struct _POLYS {
    char *poly;
    //char poly[LEN];
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

// 删除字符串中连续的等号
void delContinousEqual(char* s) {
    delBlank(s); // 删除空格
    int i = 0;
    int f = 0;
    while(*(s + i) != '\0') {
        if(*(s + i) == '=') {
            if(f == 1) {
                *(s + i) = ' ';
            }
            f = 1;
        }else {
            f = 0;
        }
        i++;
    }
    delBlank(s); // 删除空格
}

void delVar(char* poly, char* var) {
    int i = 0, left = 0, j = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '=') {
            for(j = left; j <= i; j++) {
                // 如果匹配到var，则删除var所在的项
                if(*(poly + j) == *var) {
                    int f = 1;
                    for(int t = 1; *(var + t) !='\0'; t++) {
                        if(*(poly + j + t) != *(var + t)) {
                            f = 0;
                            break;
                        }
                    }
                    /*if(*(poly + j + 1) == *(var + 1) && *(poly + j + 2) == *(var + 2) && *(poly + j + 3) == *(var + 3)
                       && *(poly + j + 4) == *(var + 4) && *(poly + j + 5) == *(var + 5)) */
                    if(f == 1){
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
    delContinousEqual(poly); // 删除可能含有的连续等号
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
                int commaNum = 0;
                for(j = 0; *(poly + left + j) != ']'; j++) {
                    if(*(poly + left + j) == ',') {
                        commaNum++;
                    }
                    if(commaNum == 3) {
                        break;
                    }
                    *(var + j) = *(poly + left + j);
                }
                /*for(j = 0; j <= 5; j++) {
                    *(var + j) = *(poly + left + j);
                }*/
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
        printf("单项： %s]=0\n", var);
    }
    //printf("%s\n", poly);
    //system("pause");

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
            int commaNum = 0, len = 0;
            for(; *(item + k + len) != ']'; len++) {
                if(*(item + k + len) == ',') {
                    commaNum++;
                }
                if(commaNum == 3) {
                    break;
                }
            }
            for(int t = k + 6; *(item + t) != '\0'; t++) {
                if(*(item + t) == '['){
                    int f = 1;
                    for(int n = 1; n < len; n++) {
                        if(*(item + k + n) != *(item + t + n)) {
                            f = 0;
                            break;
                        }
                    }
                    /*if(*(item + k + 1) == *(item + t + 1) && *(item + k + 2) == *(item + t + 2) &&
                       *(item + k + 3) == *(item + t + 3) && *(item + k + 4) == *(item + t + 4) &&
                       *(item + k + 5) == *(item + t + 5)) */
                    if(f == 1) {
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


// 判断字符串*s1 和 *s2 是否相等
int isStrEqual(char* s1, char* s2) {
    int i = 0;
    while(*(s1 + i) != '\0' && *(s2 + i) != '\0') {
        if(*(s1 + i) != *(s2 + i)) {
            return 0;
        }
        i++;
    }
    if(*(s1 + i) != '\0' || *(s2 + i) != '\0')
        return 0;
    return 1;
}

// 将字符串写入到文件中
void writeToFile(char *s) {
    FILE *pFile = fopen("result.txt", "a");
    fprintf(pFile, "%s\n", s);
    fclose(pFile);
}

// 判断链表中是否含有strLine
int isExist(char* strLine, POLYS headPolys){
    POLYS curPolys = headPolys;
    while(curPolys) {
        int i = 0, j = 0;
        int flag = 1;
        while(*(strLine + i) != '\0' && *(curPolys->poly + j) != '\0') {
            if(*(strLine + i) != *(curPolys->poly + j)) {
                flag = 0;
                break;
            }
            i++;
            j++;
            if(*(strLine + i) != '\0' && *(curPolys->poly + j) == '\0' ||
               *(strLine + i) == '\0' && *(curPolys->poly + j) != '\0') {
                flag = 0;
                break;
            }
        }
        if(flag == 1) {
            return true;
        }
        curPolys = curPolys->next;
    }
    return false;
}

int main()
{
    char polyFileName[] = "poly6.txt";  // 文件路径

    char strLine[LEN];

    FILE* pFile;
    POLYS headPolys, tailPolys, curPolys, newPolys;  // 链表首节点,尾节点,当前节点,新的节点
    tailPolys = headPolys = (POLYS)malloc(sizeof(struct _POLYS));
    headPolys->next = NULL;

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("无法打开文件");
        return -1;
    }

    fseek(pFile, 0, SEEK_END);
    int len = ftell(pFile);
    printf("%d\n", len);
    rewind(pFile);
    fread(strLine, 1, len, pFile);

    // 预处理
    delStr(strLine, (char*)"\n");
    delStr(strLine, (char*)"{");
    delStr(strLine, (char*)"=0,}");
    delStr(strLine, (char*)"=0,,");
    delStr(strLine, (char*)"Subscript");
    delStr(strLine, (char*)"Subsuperscript");
    printf("%s\n%d\n", strLine, strlen(strLine));

    len = strlen(strLine);

    headPolys->poly = (char*)malloc((len+1)*sizeof(char));
    strcpy(headPolys->poly, strLine);

    char var[VAR_LEN];  // 变量数组
    char item[LEN];
    while(headPolys != NULL){
        printf("%s\n", headPolys->poly);
        if(findVar2(headPolys->poly, item)){
            //printf("%s\n", headPolys->poly);
            printf("项: %s\n", item);
            //system("pause");
            int i = 0, j = 0;
            while(*(item + i) != '\0') {
                if(*(item + i) == '[') {
                    int commaNum = 0;
                    for(j = 0; *(item + i + j) != ']'; j++) {
                        if(*(item + i + j) == ',') {
                            commaNum++;
                        }
                        if(commaNum == 3) {
                            break;
                        }
                        *(var + j) = *(item + i + j);
                    }
                    *(var + j) = '\0';

                    /*for(j = 0; j <= 5; j++) {
                        *(var + j) = *(item + i + j);
                    }
                    *(var + j) = '\0';*/
                    printf("%s\n", var);

                    strcpy(strLine, headPolys->poly);
                    delVar(strLine, var);

                    // 如果链表不存在，则加入链表
                    if(!isExist(strLine, headPolys)) {
                        newPolys = (POLYS)malloc(sizeof(struct _POLYS));
                        newPolys->next = NULL;
                        newPolys->poly = (char*)malloc((strlen(strLine)+1)*sizeof(char));
                        strcpy(newPolys->poly, strLine);
                        //printf("%s\n", newPolys->poly);
                        //system("pause");

                        tailPolys->next = newPolys;
                        tailPolys = tailPolys->next;
                    }
                }
                i++;
            }
        }else {
            //printf("结果 ****************\n");
            writeToFile((char*)"结果 ****************");
            for(int i = 0; *(headPolys->poly + i) != '\0'; i++) {
                if(*(headPolys->poly + i) == '='){
                    *(headPolys->poly + i) = '\n';
                }
            }
            //printf("%s\n", headPolys->poly);
            //printf("*********************\n");
            writeToFile(headPolys->poly);
            writeToFile((char*)"*********************");
            //system("pause");
        }
        //system("pause");
        curPolys = headPolys;
        headPolys = headPolys->next;
        //free(curPolys->next);
        free(curPolys);
    }
}
