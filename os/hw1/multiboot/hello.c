
int next_color() {
    static int color = 0x06;
    color = (color + 0x01) % 16;
    color = (color == 0) ? 0x01 : color;
    return color;
}


void cmain(void) {
    const int COLUMN_NUM = 0xB80A0 - 0xB8000;
    const int LINE_INDEX = 11;
    const int SHIFT = 31;

    char* video = (char*) 0xB8000 + COLUMN_NUM * LINE_INDEX + SHIFT * 2;

    const char hw[] = "Hello World!";
    int i;
    for (i = 0; hw[i] != '\0'; i++) {
        video[i * 2] = hw[i];
        video[i * 2 + 1] = next_color();
    }
}