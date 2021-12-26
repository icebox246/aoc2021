// Project: day24
#include <bits/stdc++.h>
using namespace std;

typedef long long ll;

enum OpType { Inp, Add, Mul, Div, Mod, Eql };
map<string, OpType> strToOp = {{"inp", Inp}, {"add", Add}, {"mul", Mul},
                               {"div", Div}, {"mod", Mod}, {"eql", Eql}};

struct Op {
    OpType typ;
    bool withLit;
    ll lval;
    ll rval;
};

typedef vector<ll> Alu;

int main() {
    ios_base::sync_with_stdio(0);
    cin.tie(0);

    vector<Op> program;

    ifstream inpfile("input");
    while (!inpfile.eof()) {
        string ln;
        getline(inpfile, ln);
        vector<string> words;
        string buf = "";
        for (auto c : ln) {
            if (c == ' ') {
                words.push_back(buf);
                buf = "";
            } else {
                buf += c;
            }
        }
        words.push_back(buf);

        if (words.size() < 2) break;

        Op op{.typ = strToOp[words[0]], .lval = words[1][0] - 'w'};
        if (op.typ != Inp) {
            if (isalpha(words[2][0])) {
                op.rval = words[2][0] - 'w';
            } else {
                op.rval = stoi(words[2]);
                op.withLit = 1;
            }
        }

        program.push_back(op);
    }
    inpfile.close();

    cerr << "Input file reading finished!" << endl;

    map<Alu, pair<ll, ll>> alus = {{{0, 0, 0, 0}, {0, 0}}};

    ll inpCnt = 0;

    ll lastTime = time(0);

    for (Op& op : program) {
        map<Alu, pair<ll, ll>> new_alus;
        if (op.typ == Inp) {  // generate input
            for (auto alu_p : alus) {
                for (ll d = 1; d <= 9; d++) {
                    Alu alu = alu_p.first;
                    ll nmx = alu_p.second.first;
                    ll nmn = alu_p.second.second;
                    alu[0] = d;
                    if (new_alus.count(alu) <= 0)
                        new_alus[alu] = {nmx *10 + d, nmn *10 + d};
                    new_alus[alu] = {max(new_alus[alu].first, nmx * 10 + d),
                                     min(new_alus[alu].second, nmn * 10 + d)};
                }
            }
            inpCnt++;
            cerr << "Alus to process: " << alus.size() << " after " << inpCnt
                 << " inputs, last batch took: " << time(0) - lastTime << endl;
            lastTime = time(0);
        } else {  // handle arithmetics
            for (auto alu_p : alus) {
                Alu alu = alu_p.first;
                ll nmx = alu_p.second.first;
                ll nmn = alu_p.second.second;
                ll rval = op.rval;
                if (!op.withLit) rval = alu[rval];
                switch (op.typ) {
                    case Add:
                        alu[op.lval] += rval;
                        break;
                    case Mul:
                        alu[op.lval] *= rval;
                        break;
                    case Div:
                        alu[op.lval] /= rval;
                        break;
                    case Mod:
                        alu[op.lval] %= rval;
                        break;
                    case Eql:
                        alu[op.lval] = alu[op.lval] == rval;
                        break;
                    case Inp:
                        assert(false);
                        break;
                }
                if (new_alus.count(alu) <= 0)
                    new_alus[alu] = {nmx, nmn};
                new_alus[alu] = {max(new_alus[alu].first, nmx),
                                 min(new_alus[alu].second, nmn)};
            }
        }
        alus = new_alus;
    }

    ll mx = 0, mn = LLONG_MAX;

    for (auto alu_p : alus) {
        if (alu_p.first[3] == 0) {
            mx = max(mx, alu_p.second.first);
            mx = max(mx, alu_p.second.second);
            mn = min(mn, alu_p.second.first);
            mn = min(mn, alu_p.second.second);
        }
    }

    cout << "Part I:" << mx << endl << "Part II:" << mn << endl;
}
