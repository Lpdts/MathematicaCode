#include <stdio.h>
#include <stdlib.h>
#include <string>
using namespace std;

int main()
{
    char polyFileName[] = "poly2.txt";  // 文件路径

    FILE* pFile;
    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("无法打开文件");
        return -1;
    }



    // 预处理
    delStr(headPolys->poly, (char*)"\n");
    delStr(headPolys->poly, (char*)"{");
    delStr(headPolys->poly, (char*)"=0,}");
    delStr(headPolys->poly, (char*)"=0,,");
    delStr(headPolys->poly, (char*)"Subscript");
    delStr(headPolys->poly, (char*)"Subsuperscript");
    printf("%s\n%d\n", headPolys->poly, strlen(headPolys->poly));

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
                    newPolys = (POLYS)malloc(sizeof(struct _POLYS));
                    newPolys->next = NULL;

                    strcpy(newPolys->poly, headPolys->poly);
                    delVar(newPolys->poly, var);
                    //printf("%s\n", newPolys->poly);
                    //system("pause");

                    tailPolys->next = newPolys;
                    tailPolys = tailPolys->next;
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
