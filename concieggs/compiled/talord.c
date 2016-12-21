#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void talord(char *num)
{
    int len = strlen(num);

    if (len == 0 || len > 4) {
        printf("%s", num);
        return;
    }

    char *single_digits[] =
        { "nul", "et", "to", "tre", "fire",
          "fem", "seks", "syv", "otte", "ni" };

    char *two_digits[] =
        { "", "ti", "elleve", "tolv", "tretten", "fjorten",
          "femten", "seksten", "sytten", "atten", "nitten" };

    char *tens_multiple[] =
        { "", "", "tyve", "tredive", "fyrre", "halvtreds",
          "treds", "halvfjerds", "firs", "halvfems" };

    char *tens_power[] = { "hundrede", "tusinde" };

    if (len == 1) {
        printf("%s", single_digits[*num - '0']);
        return;
    }

    while (*num != '\0') {
        if (len >= 3) {
            if (*num - '0' != 0) {
                printf("%s", single_digits[*num - '0']);
                printf("%s ", tens_power[len-3]);
            }
            --len;
        }

        else {
            if (*num == '1') {
                int sum = *num - '0' + *(num + 1) - '0';
                printf("%s", two_digits[sum]);
                return;
            }

            else if (*num == '2' && *(num + 1) == '0') {
                printf("tyve");
                return;
            }

            else {
                int i = *num - '0';
                int j = *(num + 1) - '0';
                printf("%s%s%s", j ? single_digits[j] : "",
                                 j ? "og" : "",
                                 i ? tens_multiple[i] : "");
                return;
            }
        }
        ++num;
    }
}

/* Driver program to test above function */
int main(int argc, char *argv[])
{
    if (argc == 2) {
        talord(argv[1]);
        return 0;
    }

    return 1;
}
