#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 2048   // ����strLine �ĳ���
#define VAR_LEN 8  // �����ĳ���

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
            left = i;   // ��¼�������ŵ�λ��
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
    // �����ڣ�����0
    return 0;
}

int main()
{
    char polyFileName[] = "";

    FILE* pFile;
    char strLine[LEN];

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("�޷����ļ�");
        return -1;
    }

    fgets(strLine, LEN, pFile);
    int len = strlen(strLine);   // �ַ���strLine �ĳ���

    // ɾ���ַ����Ŀո�
    delBlank(strLine);

    char var[VAR_LEN];  // ��������

    while(findVar(strLine, var)) {
        delVar(strLine, var);
    }
}
