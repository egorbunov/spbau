int next_color() {
    const int COLOR_NUM = 16;
    static int color = 0x06;
    color = (color + 0x01) % COLOR_NUM;
    color = (color == 0) ? 0x01 : color;
    return color;
}

void cmain(void) {
    const int COLUMN_NUM = 0xA0; // length of line (80 symbols)
    const int LINE_INDEX = 11;
    const int SHIFT = 31;
    const char str[] = "Hello, World!";

    char* video = (char*) 0xB8000 + COLUMN_NUM * LINE_INDEX + SHIFT * 2;
    int i;
    for (i = 0; str[i] != '\0'; i++) {
        video[i * 2] = str[i];
        video[i * 2 + 1] = next_color();
    }
}