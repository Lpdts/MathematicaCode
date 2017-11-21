#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define LEN 2048000  //12650 // ����strLine �ĳ���
#define VAR_LEN 16  // �����ĳ���

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

// ɾ���ַ����������ĵȺ�
void delContinousEqual(char* s) {
    delBlank(s); // ɾ���ո�
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
    delBlank(s); // ɾ���ո�
}

void delVar(char* poly, char* var) {
    int i = 0, left = 0, j = 0;
    while(*(poly + i) != '\0') {
        if(*(poly + i) == '=') {
            for(j = left; j <= i; j++) {
                // ���ƥ�䵽var����ɾ��var���ڵ���
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
    delBlank(poly);  // ɾ������var�������ɾ���ո�
    delContinousEqual(poly); // ɾ�����ܺ��е������Ⱥ�
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
    // �����ڣ�����0
    return 0;
}

// �ҵ����� xy = 0 ����
int findVar2(char* poly, char* item) {

    char var[VAR_LEN];  // ��������
    // ɾ��x^n = 0 ����
    while(findVar(poly, var)) {
        delVar(poly, var);
        printf("��� %s]=0\n", var);
    }
    //printf("%s\n", poly);
    //system("pause");

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
    delBlank(item);  // ɾ���ո�
    return value;
}


// �ж��ַ���*s1 �� *s2 �Ƿ����
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

// �����м������ļ���
void saveStrToFile(char *s) {
    FILE *pFile = fopen("temp.txt", "a");
    fprintf(pFile, "%s\n", s);
    fflush(pFile);
    fclose(pFile);
}
// ���ļ��ж�ȡ�м���
void getStrFromFile(char *s, int line) {
    FILE *pFile = fopen("temp.txt", "r");
    while(line--) {
        fgets(s, LEN, pFile);
    }
    fflush(pFile);
    fclose(pFile);
}

// ������ַ���д�뵽�ļ���
void writeToFile(char *s) {
    FILE *pFile = fopen("result.txt", "a");
    fprintf(pFile, "%s\n", s);
    fclose(pFile);
}

int main()
{
    char polyFileName[] = "poly2.txt";  // �ļ�·��

    FILE* pFile;
    char strLine[LEN];
    char strTemp[LEN];

    if((pFile = fopen(polyFileName, "r")) == NULL) {
        printf("�޷����ļ�");
        return -1;
    }

    fseek(pFile, 0, SEEK_END);
    int len = ftell(pFile);
    printf("%d\n", len);
    rewind(pFile);
    fread(strLine, 1, len, pFile);

    // Ԥ����
    delStr(strLine, (char*)"\n");
    delStr(strLine, (char*)"{");
    delStr(strLine, (char*)"=0,}");
    delStr(strLine, (char*)"=0,,");
    delStr(strLine, (char*)"Subscript");
    delStr(strLine, (char*)"Subsuperscript");
    printf("%s\n%d\n", strLine, strlen(strLine));

    // �������뵽�м��ļ�
    saveStrToFile(strLine);

    char var[VAR_LEN];  // ��������
    char item[1000];
    int curLine = 1, allLine = 1;
    while(curLine <= allLine) {
        getStrFromFile(strLine, curLine);
        printf("%d\n", findVar2(strLine, item));
        if(findVar2(strLine, item)){
            //printf("%s\n", strLine);
            printf("��: %s\n", item);
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
                    printf("%s\n", var);
                    strcpy(strTemp, strLine);
                    delVar(strTemp, var);
                    saveStrToFile(strTemp);
                    allLine++;
                }
                i++;
            }
        }else {
            writeToFile((char*)"��� ****************");
            for(int i = 0; *(strLine + i) != '\0'; i++) {
                if(*(strLine + i) == '='){
                    *(strLine + i) = '\n';
                }
            }
            writeToFile(strLine);
            writeToFile((char*)"*********************");
            //system("pause");
        }
        curLine++;
    }
    return 0;
}
