#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 2048   // ����strLine �ĳ���
#define VAR_LEN 8  // �����ĳ���

typedef struct _POLYS {
    char poly[LEN];
    struct _POLYS *next;
}* POLYS;

//ɾ���ַ���*a�еĿո�
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

// ���ַ���*poly��
void delStr(char *poly, char* str){

}

void delVar(char* poly, char* var) {
    int i = 0, left = 0, j = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '=') {
            for(j = left; j <= i; j++) {
                // ���ƥ�䵽var����ɾ��var���ڵ���
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
    delBlank(poly);  // ɾ������var�������ɾ���ո�
}

// �ҵ����� x^n = 0 ����
int findVar(char* poly, char* var) {
    int i = 0, left = 0, j = 0;
    int numOfBracket = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '[') {
            numOfBracket++;
            left = i;   // ��¼�������ŵ�λ��
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
    // �����ڣ�����0
    return 0;
}

// �ҵ����� xy = 0 ����
int findVar2(char* poly, char* item) {
    int i = 0, j = 0, left = 0;  // xy=0������
    int numOfPlusSub = 0;  // +,-�ĸ�����ȥ����һ�������Ϊ����Ϊxy=0����
    int value = 0;  // ��������ֵ

    while(*(poly + i) != '\0') {
        if((*(poly + i) == '+' || *(poly + i) == '-') && i != 0 && *(poly + i - 1) != '=') {
            numOfPlusSub++;
        }
        if(*(poly + i) == '=' && *(poly + i - 1) != '=') { // ȥ��==�����
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
    // todo ȥ��
    return value;
}

int main()
{
    char polyFileName[] = "poly.txt";

    FILE* pFile;
    //char strLine[LEN];
    POLYS headPolys, tailPolys, curPolys, newPolys;  // �����׽ڵ�,β�ڵ�,��ǰ�ڵ�,�µĽڵ�
    tailPolys = headPolys = (POLYS)malloc(sizeof(struct _POLYS));
    headPolys->next = NULL;

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("�޷����ļ�");
        return -1;
    }

    fgets(headPolys->poly, LEN, pFile);
    //printf("%s\n", strLine);

    int len = strlen(headPolys->poly);   // �ַ���strLine �ĳ���

    // ɾ���ַ����Ŀո�
    delBlank(headPolys->poly);

    printf("%s\n", headPolys->poly);

    char var[VAR_LEN];  // ��������

    while(findVar(headPolys->poly, var)) {
        delVar(headPolys->poly, var);
        printf("%s]=0\n", var);
    }
    printf("%s\n", headPolys->poly);

    char item[100];
    while(headPolys != NULL){
        if(findVar2(headPolys->poly, item)){
            printf("��: %s\n", item);
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
