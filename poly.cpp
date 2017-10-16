#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <vector>
using namespace std;

#define LEN 2048   // ����strLine �ĳ���
#define VAR_LEN 8  // �����ĳ����Ϊ8


// �����Ľṹ��
typedef struct _variable{
    char buf[VAR_LEN];
}Var;


typedef struct _variablePoly{
    Var var;
    int exp;     // ָ��
}VarPoly;


typedef struct _polyitem{
    vector<VarPoly> varPolys;
    int coef;    // ϵ��
}PolyItem;


typedef struct _poly{
    vector<PolyItem> polyItems;
}Poly;

Var atovar(char buf[]) {
    Var var;
    int i = 0, j = 0;
    while(buf[i] != '\0') {
        if(buf[i] != ' ') {
            var.buf[j++] = buf[i++];
        }else {
            i++;
        }
    }
    var.buf[j++] = '\0';
    return var;
}

// �ж��Ƿ���ϵ��
int isCoef(char a) {
    if(a == '-' || a == '+' || a >= '0' && a <= '9')
        return 1;
    else
        return 0;
}

Poly atopoly(char buf[]) {
    Poly poly;
    int i = 0;
    while(buf[i] != '\0') {
        PolyItem polyItem;
        if(isCoef(buf[i])) {
            char* coeftemp;
            int j = 0;
            while(isCoef(buf[i])) {
                *(coeftemp + j) = buf[i++];
                j++;
            }
            *(coeftemp + j) = '\0';
            polyItem.coef = atoi(coeftemp);  //todo

            while(buf[i] != '\0') {
                if(isCoef(buf[i])){
                    break;
                }

                if(buf[i] == '[') {
                    char *vartemp;
                    j = 0; i++;
                    while(buf[i] != ']') {
                        *(vartemp + j) = buf[i++];
                        j++;
                    }
                    *(vartemp + j) = '\0';
                    //todo
                }
            }
        }
        poly.polyItems.push_back(polyItem);
        //i++;
    }

    return poly;
}

int main()
{
    char polyFileName[] = "C:\\Users\\Admin\\Desktop\\poly.txt";
    char varFileName[] = "C:\\Users\\Admin\\Desktop\\var.txt";

    FILE* pFile;
    char strLine[LEN];

    // ��ȡ����ʽ����
    vector<Poly> polys;   // ����ʽ����
    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("�޷���polyFile");
        return -1;
    }

    while(!feof(pFile)) {
        fgets(strLine, LEN, pFile);
        polys.push_back(atopoly(strLine));
        //printf("%d\n", strlen(strLine));
        //printf("%s\n", strLine);
    }

    fclose(pFile);

    // ��ȡ����
    /*vector<Var> vars;  // ��������
    if((pFile = fopen(varFileName, "r")) == NULL) {
        printf("�޷���varFile");
        return -1;
    }
    fgets(strLine, LEN, pFile);
    int i = 0, j = 0;
    while(i < strlen(strLine)) {
        if(strLine[i] == '[') {
            j = 0; i++;
            char* buf;
            while(strLine[i] != ']') {
                *(buf + j) = strLine[i++];
                j++;
            }
            *(buf + j) = '\0';
            vars.push_back(atovar(buf));
        }
        i++;
    }
    fclose(pFile);*/

    return 0;
}


