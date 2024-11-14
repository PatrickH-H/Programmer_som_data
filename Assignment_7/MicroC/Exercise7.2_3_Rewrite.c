void histogram(int n, int ns[], int max, int freq[]) {
    int i;
    for(i=0; i<n; i = i+1) {
        freq[i] = 0;  
    }

    for(i=0; i<n; i = i+1) {
        if (ns[i] >= 0 && ns[i] <= max) {
            freq[ns[i]] = freq[ns[i]] + 1;  
        }
    }
}

int main(int n) {
    int n;
    n = 7; 
    int max;
    max = 3;

    int arr[7]; 
    arr[0] = 1;
    arr[1] = 2;
    arr[2] = 1;
    arr[3] = 1;
    arr[4] = 1;
    arr[5] = 2;
    arr[6] = 0;

    int freq[8];


    histogram(n, arr, max, freq);


    int j;
    j = 0;
    while (j <= max) {
        print freq[j];
        j = j + 1;
    }
}
