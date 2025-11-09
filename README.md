#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>

#if defined(_WIN32)
  #include <direct.h>
  #define MKDIR(p, m) _mkdir(p)
  #define PATHSEP '\\'
#else
  #include <sys/stat.h>
  #define MKDIR(p, m) mkdir(p, m)
  #define PATHSEP '/'
#endif

#include <dirent.h>

#define STUDENT_ID "241ADB088"
#define NAME       "Navaneeth"
#define LASTNAME   "Reddy"

typedef long long i64;

static size_t first_error_pos = 0;
static void fail(size_t pos) { if (first_error_pos == 0) first_error_pos = pos; }

typedef enum {
    T_EOF, T_INT, T_PLUS, T_MINUS, T_STAR, T_SLASH, T_POW, T_LPAREN, T_RPAREN, T_INVALID
} TokKind;

typedef struct {
    TokKind kind;
    i64     ival;
    size_t  pos1;
} Token;

typedef struct {
    const char *buf;
    size_t len, i;
    size_t abspos;
    int at_line_start;
} Lex;

static void lex_init(Lex *L, const char *buf, size_t len, size_t initial_abspos) {
    L->buf = buf; L->len = len; L->i = 0; L->abspos = initial_abspos; L->at_line_start = 1;
}
static int  lex_peek(Lex *L) { return (L->i < L->len) ? (unsigned char)L->buf[L->i] : EOF; }
static int  lex_getc(Lex *L) {
    if (L->i >= L->len) return EOF;
    unsigned char c = (unsigned char)L->buf[L->i++];
    L->abspos++;
    if (c == '\n') L->at_line_start = 1;
    return (int)c;
}
static void lex_skip_ws_and_comments(Lex *L) {
    for (;;) {
        size_t save_i = L->i, save_abs = L->abspos; int save_bol = L->at_line_start;
        for (;;) {
            int c = lex_peek(L);
            if (c==' ' || c=='\t' || c=='\r') { lex_getc(L); continue; }
            if (c=='\n') { lex_getc(L); continue; }
            break;
        }
        int c = lex_peek(L);
        if (L->at_line_start && c == '#') {
            while ((c = lex_peek(L)) != EOF && c != '\n') lex_getc(L);
            if (c == '\n') lex_getc(L);
            continue;
        }
        if (L->i == save_i && L->abspos == save_abs && L->at_line_start == save_bol) break;
    }
}
static Token next_token(Lex *L) {
    lex_skip_ws_and_comments(L);
    size_t start_pos = L->abspos + 1;
    int c = lex_peek(L);
    if (c == EOF) return (Token){ .kind=T_EOF, .pos1 = L->abspos + 1 };
    if (c == '*') {
        if (L->i + 1 < L->len && L->buf[L->i+1] == '*') {
            lex_getc(L); lex_getc(L);
            L->at_line_start = 0;
            return (Token){ .kind=T_POW, .pos1=start_pos };
        }
        lex_getc(L); L->at_line_start = 0; return (Token){ .kind=T_STAR, .pos1=start_pos };
    }
    if (c == '+') { lex_getc(L); L->at_line_start = 0; return (Token){ .kind=T_PLUS,  .pos1=start_pos }; }
    if (c == '-') { lex_getc(L); L->at_line_start = 0; return (Token){ .kind=T_MINUS, .pos1=start_pos }; }
    if (c == '/') { lex_getc(L); L->at_line_start = 0; return (Token){ .kind=T_SLASH, .pos1=start_pos }; }
    if (c == '(') { lex_getc(L); L->at_line_start = 0; return (Token){ .kind=T_LPAREN,.pos1=start_pos }; }
    if (c == ')') { lex_getc(L); L->at_line_start = 0; return (Token){ .kind=T_RPAREN,.pos1=start_pos }; }
    if (isdigit(c)) {
        i64 v = 0;
        while (isdigit(lex_peek(L))) { c = lex_getc(L); v = v*10 + (c - '0'); }
        L->at_line_start = 0;
        return (Token){ .kind=T_INT, .ival=v, .pos1=start_pos };
    }
    lex_getc(L); L->at_line_start = 0;
    return (Token){ .kind=T_INVALID, .pos1=start_pos };
}

typedef struct { Token *toks; size_t n, p; } Parser;
static Token tok_at(Parser *P) {
    if (P->p < P->n) return P->toks[P->p];
    return (Token){ .kind=T_EOF, .pos1=(P->n ? P->toks[P->n-1].pos1 : 1)+1 };
}
static Token consume(Parser *P) { return (P->p < P->n) ? P->toks[P->p++] : tok_at(P); }
static i64 parse_expr(Parser *P);
static i64 parse_primary(Parser *P) {
    Token t = tok_at(P);
    if (t.kind == T_INT) { consume(P); return t.ival; }
    if (t.kind == T_LPAREN) {
        consume(P);
        i64 v = parse_expr(P);
        if (first_error_pos) return 0;
        Token r = tok_at(P);
        if (r.kind != T_RPAREN) { fail(r.pos1); return 0; }
        consume(P);
        return v;
    }
    fail(t.pos1);
    return 0;
}
static i64 ipow_checked(i64 base, i64 exp, size_t exp_pos) {
    if (exp < 0) { fail(exp_pos); return 0; }
    i64 result = 1, b = base;
    while (exp > 0) {
        if (exp & 1) result = result * b;
        exp >>= 1;
        if (exp) b = b * b;
    }
    return result;
}
static i64 parse_power(Parser *P) {
    i64 left = parse_primary(P);
    if (first_error_pos) return 0;
    Token t = tok_at(P);
    if (t.kind == T_POW) {
        consume(P);
        Token rhs_first = tok_at(P);
        i64 right = parse_power(P);
        if (first_error_pos) return 0;
        left = ipow_checked(left, right, rhs_first.pos1);
    }
    return left;
}
static i64 parse_term(Parser *P) {
    i64 acc = parse_power(P);
    if (first_error_pos) return 0;
    for (;;) {
        Token t = tok_at(P);
        if (t.kind == T_STAR) {
            consume(P);
            i64 rhs = parse_power(P);
            if (first_error_pos) return 0;
            acc *= rhs;
        } else if (t.kind == T_SLASH) {
            consume(P);
            Token divtok = tok_at(P);
            i64 rhs = parse_power(P);
            if (first_error_pos) return 0;
            if (rhs == 0) { fail(divtok.pos1); return 0; }
            acc /= rhs;
        } else break;
    }
    return acc;
}
static i64 parse_expr(Parser *P) {
    i64 acc = parse_term(P);
    if (first_error_pos) return 0;
    for (;;) {
        Token t = tok_at(P);
        if (t.kind == T_PLUS) {
            consume(P);
            i64 rhs = parse_term(P);
            if (first_error_pos) return 0;
            acc += rhs;
        } else if (t.kind == T_MINUS) {
            consume(P);
            i64 rhs = parse_term(P);
            if (first_error_pos) return 0;
            acc -= rhs;
        } else break;
    }
    return acc;
}

static int eval_buffer(const char *buf, size_t len, size_t base_abspos, i64 *out) {
    size_t cap = 64, n = 0;
    Token *toks = (Token*)malloc(cap * sizeof(Token));
    if (!toks) { fprintf(stderr, "OOM\n"); exit(1); }
    Lex L; lex_init(&L, buf, len, base_abspos);
    for (;;) {
        Token tk = next_token(&L);
        if (tk.kind == T_INVALID && first_error_pos == 0) fail(tk.pos1);
        if (n == cap) {
            cap *= 2;
            toks = (Token*)realloc(toks, cap * sizeof(Token));
            if (!toks) { fprintf(stderr, "OOM\n"); exit(1); }
        }
        toks[n++] = tk;
        if (tk.kind == T_EOF) break;
    }
    if (!first_error_pos) {
        Parser P = { .toks=toks, .n=n, .p=0 };
        i64 result = parse_expr(&P);
        if (!first_error_pos) {
            Token t = tok_at(&P);
            if (t.kind != T_EOF) fail(t.pos1);
            else { *out = result; free(toks); return 1; }
        }
    }
    free(toks);
    return 0;
}

static void join_path(char *out, size_t outsz, const char *a, const char *b) {
    size_t la = strlen(a);
    int need_sep = (la > 0 && a[la-1] != '/' && a[la-1] != '\\');
    if (need_sep) snprintf(out, outsz, "%s%c%s", a, PATHSEP, b);
    else          snprintf(out, outsz, "%s%s", a, b);
}
static const char *basename_no_ext(const char *path) {
    const char *slash = strrchr(path, PATHSEP);
#if defined(_WIN32)
    const char *slash2 = strrchr(path, '/');
    if (!slash || (slash2 && slash2 > slash)) slash = slash2;
#endif
    const char *base = slash ? slash + 1 : path;
    const char *dot = strrchr(base, '.');
    static char name[512];
    if (dot && dot > base) {
        size_t n = (size_t)(dot - base);
        if (n >= sizeof(name)) n = sizeof(name) - 1;
        memcpy(name, base, n); name[n] = '\0';
    } else {
        strncpy(name, base, sizeof(name)); name[sizeof(name)-1] = '\0';
    }
    return name;
}
static const char *dir_base(const char *path) {
    size_t len = strlen(path);
    while (len && (path[len-1]=='/' || path[len-1]=='\\')) len--;
    if (!len) return "dir";
    const char *end = path + len;
    const char *p = end - 1;
    while (p >= path && *p != '/' && *p != '\\') p--;
    p++;
    static char name[512];
    size_t n = (size_t)(end - p);
    if (n >= sizeof(name)) n = sizeof(name)-1;
    memcpy(name, p, n); name[n]='\0';
    return name;
}
static const char *get_username() {
#if defined(_WIN32)
    const char *u = getenv("USERNAME");
#else
    const char *u = getenv("USER");
#endif
    return u && *u ? u : "user";
}
static int ensure_dir_exists(const char *path) {
    if (MKDIR(path, 0775) == 0) return 1;
    if (errno == EEXIST) return 1;
    return 0;
}
static void default_outdir(char *dst, size_t dstsz, const char *input_base) {
    snprintf(dst, dstsz, "%s_%s_%s", input_base, get_username(), STUDENT_ID);
}
static void build_outname(char *dst, size_t dstsz, const char *base) {
    snprintf(dst, dstsz, "%s_%s_%s_%s.txt", base, NAME, LASTNAME, STUDENT_ID);
}

static void write_result_file(const char *outdir, const char *inpath, int ok, i64 val, size_t errpos) {
    char base[512]; strncpy(base, basename_no_ext(inpath), sizeof(base)); base[sizeof(base)-1]='\0';
    char outname[1024]; build_outname(outname, sizeof(outname), base);
    char outpath[1500]; join_path(outpath, sizeof(outpath), outdir, outname);
    FILE *f = fopen(outpath, "wb");
    if (!f) { fprintf(stderr, "Cannot write %s: %s\n", outpath, strerror(errno)); return; }
    if (ok) fprintf(f, "%lld\n", (long long)val);
    else    fprintf(f, "ERROR:%zu\n", errpos);
    fclose(f);
    fprintf(stderr, "Wrote %s\n", outpath);
}
static int process_single_file(const char *inpath, const char *outdir) {
    FILE *f = fopen(inpath, "rb");
    if (!f) { fprintf(stderr, "cannot open %s: %s\n", inpath, strerror(errno)); return 0; }
    if (fseek(f, 0, SEEK_END) != 0) { fclose(f); fprintf(stderr,"seek failed\n"); return 0; }
    long sz = ftell(f); if (sz < 0) { fclose(f); fprintf(stderr,"ftell failed\n"); return 0; }
    rewind(f);
    char *buf = (char*)malloc((size_t)sz + 1);
    if (!buf) { fclose(f); fprintf(stderr,"OOM\n"); return 0; }
    size_t len = fread(buf, 1, (size_t)sz, f);
    fclose(f);
    buf[len] = '\0';
    first_error_pos = 0;
    i64 result = 0;
    int ok = eval_buffer(buf, len, 0, &result);
    write_result_file(outdir, inpath, ok, result, first_error_pos ? first_error_pos : (len + 1));
    free(buf);
    return ok;
}
static int has_txt(const char *name) {
    size_t L = strlen(name);
    return (L >= 4 && strcmp(name + L - 4, ".txt") == 0);
}
static int process_directory(const char *indir, const char *outdir) {
    DIR *d = opendir(indir);
    if (!d) { fprintf(stderr, "cannot open dir %s: %s\n", indir, strerror(errno)); return 0; }
    int all_ok = 1; struct dirent *e;
    while ((e = readdir(d)) != NULL) {
        const char *n = e->d_name;
        if (strcmp(n,".")==0 || strcmp(n,"..")==0) continue;
        if (!has_txt(n)) continue;
        char inpath[1500]; join_path(inpath, sizeof(inpath), indir, n);
        if (!process_single_file(inpath, outdir)) all_ok = 0;
    }
    closedir(d);
    return all_ok;
}

int main(int argc, char **argv) {
    const char *dir = NULL, *outdir = NULL, *single = NULL;
    for (int i = 1; i < argc; ++i) {
        if ((strcmp(argv[i], "-d")==0 || strcmp(argv[i], "--dir")==0) && i+1 < argc) dir = argv[++i];
        else if ((strcmp(argv[i], "-o")==0 || strcmp(argv[i], "--output-dir")==0) && i+1 < argc) outdir = argv[++i];
        else single = argv[i];
    }
    char defout[1024];
    if (!outdir) {
        const char *base = dir ? dir_base(dir) : basename_no_ext(single ? single : "input");
        default_outdir(defout, sizeof(defout), base);
        outdir = defout;
    }
    if (!ensure_dir_exists(outdir)) {
        fprintf(stderr, "Failed to create/access output dir: %s\n", outdir);
        return 2;
    }
    if (dir && single) {
        fprintf(stderr, "Usage: %s [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt\n", argv[0]);
        return 1;
    }
    if (dir) return process_directory(dir, outdir) ? 0 : 3;
    if (!single) {
        fprintf(stderr, "Usage: %s [-d DIR|--dir DIR] [-o OUTDIR|--output-dir OUTDIR] input.txt\n", argv[0]);
        return 1;
    }
    return process_single_file(single, outdir) ? 0 : 4;
}
