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

// ���ַ���*poly�� ɾ���ַ���*str
void delStr(char *poly, char* str){
    delBlank(poly);   // ɾ���ո�
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
    delBlank(poly);  // ɾ���ո�
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

    char var[VAR_LEN];  // ��������
    // ɾ��x^n = 0 ����
    while(findVar(poly, var)) {
        delVar(poly, var);
        printf("%s]=0\n", var);
    }
    printf("%s\n", poly);
    system("pause");

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
    // ȥ��
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
    delBlank(item);  // ɾ���ո�
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
    printf("%s\n", headPolys->poly);

    delStr(headPolys->poly, "Subscript");    //Subscript   Subsuperscript
    delStr(headPolys->poly, "Subsuperscript");
    printf("%s\n", headPolys->poly);

    int len = strlen(headPolys->poly);   // �ַ���strLine �ĳ���

    char var[VAR_LEN];  // ��������
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
