#include <stdio.h>
#include <stdlib.h>
#include <string>
#include <fstream>
#include <iostream>
using namespace std;

//ɾ���ַ���s�е�d
void delStr(string& s, string d) {
    while(s.find(d) != string::npos) {
        s.erase(s.find(d), d.length());
    }
}

// ɾ���ַ����������ĵȺ�
void delContinousEqual(string& s) {
    delStr(s, " "); // ɾ���ո�
    int i = 0;
    int f = 0;
    while(i < s.length()) {
        if(s[i] == '=') {
            if(f == 1) {
                s[i] = ' ';
            }
            f = 1;
        }else {
            f = 0;
        }
        i++;
    }
    delStr(s, " "); // ɾ���ո�
}

void delVar(string& poly, string& var) {
    int i = 0, left = 0, j = 0;
    while(i < poly.length() && poly[i] != '#') {
        if(poly[i] == '=') {
            for(j = left; j <= i; j++) {
                // ���ƥ�䵽var����ɾ��var���ڵ���
                if(poly[j] == var[0]) {
                    int f = 1;
                    for(int t = 1; t < var.length(); t++) {
                        if(poly[j + t] != var[t]) {
                            f = 0;
                            break;
                        }
                    }

                    if(f == 1) {
                        int k = j;
                        while(poly[k] != '=' && poly[k] != '+' && poly[k] != '-') {
                            poly[k] = ' ';
                            k++;
                        }
                        k = j;
                        while(k != left - 1) {
                            if(poly[k] == '+' || poly[k] == '-') {
                                poly[k] =  ' ';
                                break;
                            }
                            poly[k] = ' ';
                            k--;
                        }
                    }
                }
            }
            left = j;
        }
        i++;
    }
    delStr(poly, " ");  // ɾ������var�������ɾ���ո�
    delContinousEqual(poly); // ɾ�����ܺ��е������Ⱥ�
}

// �ҵ����� x^n = 0 ����
int findVar(string& oriPoly, string& var) {
    //cout<<poly<<endl;
    string poly = oriPoly.substr(0, oriPoly.find_first_of("#"));

    int i = 0, left = 0, j = 0;
    int numOfBracket = 0;
    while(i < poly.length() && poly[i] != '#') {
        if(poly[i] == '[') {
            numOfBracket++;
            left = i;   // ��¼�������ŵ�λ��
        }
        if(poly[i] == '=') {
            if(numOfBracket == 1) {
                int commaNum = 0;
                for(j = 0; poly[left + j] != ']'; j++) {
                    if(poly[left + j] == ',') {
                        commaNum++;
                    }
                    if(commaNum == 3) {
                        break;
                    }
                    var += poly[left + j];
                }
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
int findVar2(string& oriPoly, string& item) {
    string var;  // ��������
    // ɾ��x^n = 0 ����
    while(findVar(oriPoly, var)) {
        delVar(oriPoly, var);
        cout<<"���"<<var<<"]=0"<<endl;
        //cout<<oriPoly<<endl;
    }

    string poly = oriPoly.substr(0, oriPoly.find_first_of("#"));

    int i = 0, j = 0, left = 0;  // xy=0������
    int numOfPlusSub = 0;  // +,-�ĸ�����ȥ����һ�������Ϊ����Ϊxy=0����
    int value = 0;  // ��������ֵ

    while(i < poly.length() && poly[i] != '#') {
        if((poly[i] == '+' || poly[i] == '-') && i != 0 && poly[i - 1] != '=') {
            numOfPlusSub++;
        }

        if(poly[i] == '=' && poly[i - 1] != '=') { // ȥ��==�����
            if(numOfPlusSub == 0) {
                for(int k = 0; left + k <= i; k++) {
                    item += poly[left + k];
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

    // ȥ��
    for(int k = 0; k < item.length(); k++) {
        if(item[k] == '[') {
            int commaNum = 0, len = 0;
            for(; item[k + len] != ']'; len++) {
                if(item[k + len] == ',') {
                    commaNum++;
                }
                if(commaNum == 3) {
                    break;
                }
            }
            for(int t = k + 6; t < item.length(); t++) {
                if(item[t] == '['){
                    int f = 1;
                    for(int n = 1; n < len; n++) {
                        if(item[k + n] != item[t + n]) {
                            f = 0;
                            break;
                        }
                    }
                    if(f == 1) {
                        int m;
                        for(m = t; item[m] != ']'; m++) {
                            item[m] = ' ';
                        }
                        item[m] = ' ';
                    }
                }
            }
        }
    }
    delStr(item, " ");  // ɾ���ո�
    return value;

}

int main()
{
    char polyFileName[] = "poly6.txt";  // �ļ�·��

    ifstream inputFile(polyFileName);
    ofstream outFile("result.txt");   // �������ļ�

    string strLine, temp;

    if(!inputFile.is_open()) {
        printf("%s\n", "δ���ļ�");
    }

    while(getline(inputFile, temp)){
        strLine += temp + "\n";
    }

    // Ԥ����
    delStr(strLine, " ");
    delStr(strLine, "\n");
    delStr(strLine, "{");
    delStr(strLine, "=0,}");
    delStr(strLine, "=0,,");
    delStr(strLine, "Subscript");
    delStr(strLine, "Subsuperscript");

    cout<<strLine<<endl<<strLine.length()<<endl;

    strLine += "#";

    string var;  // �����ַ���
    string item;
    while(strLine.length() > 0) {
        if(findVar2(strLine, item)){
            cout<<"��: "<<item<<endl;
            system("pause");
            int i = 0, j = 0;
            while(i < item.length()) {
                if(item[i] == '[') {
                    int commaNum = 0;
                    for(j = 0; item[i + j] != ']'; j++) {
                        if(item[i + j] == ',') {
                            commaNum++;
                        }
                        if(commaNum == 3) {
                            break;
                        }
                        var += item[i + j];
                    }
                    //cout<<var<<endl;

                    int loc = strLine.find_first_of("#");
                    temp = strLine.substr(0, loc + 1);
                    delVar(temp, var);
                    //cout<<temp<<endl;
                    strLine += temp;

                    var.clear();
                }
                i++;
            }
        }else {
            int loc = strLine.find_first_of("#");
            outFile<<"���********"<<endl;
            string sub = strLine.substr(0, loc);
            int left = 0, right = 0;
            while(right < sub.length()) {
                if(sub[right] == '=') {
                    outFile<<sub.substr(left, right - left)<<endl;
                    left = right + 1;
                }
                right++;
            }
            outFile<<"************"<<endl;
        }
        int loc = strLine.find_first_of("#");
        //cout<<strLine<<endl<<strLine.length()<<endl;
        strLine.erase(0, loc + 1);
        //cout<<strLine<<endl<<strLine.length()<<endl;
        item.clear();
        system("pause");
    }

    outFile.close();

    return 0;
}
