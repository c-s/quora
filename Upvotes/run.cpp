#include <iostream>
#include <string>
#include <sstream>
#include <utility>
#include <vector>
#include <assert.h>

std::pair<int, int> get_n_k();
std::vector<int> get_upvotes();
int calculate_difference(const std::vector<int> &n, int k, int pos);

int main()
{
  auto nk = get_n_k();
  auto upvotes = get_upvotes();
  int n = nk.first;
  int k = nk.second;
  assert(1 <= n && n <= 100000);
  assert(1 <= k && k <= n);
  assert(upvotes.size() == n);
  for(int pos=0; pos<n-k+1; ++pos) {
    int difference = calculate_difference(upvotes, k, pos);
    std::cout << difference << std::endl;
  }
  return 0;
}

// returns the pair (n, k) from stdin.
std::pair<int, int> get_n_k()
{
  std::string buf;
  std::getline(std::cin, buf);

  std::istringstream ss_buf(buf);
  std::string n_str;
  std::string k_str;
  // assume the input format is as expected.
  std::getline(ss_buf, n_str, ' ');
  std::getline(ss_buf, k_str, ' ');
  int n, k;
  std::istringstream(n_str) >> n;
  std::istringstream(k_str) >> k;
  return std::make_pair(n, k);
}

// returns a vector of upvote numbers from stdin.
std::vector<int> get_upvotes()
{
  std::string buf;
  std::getline(std::cin, buf);

  std::istringstream ss_buf(buf);
  std::string one_num_str;
  std::vector<int> upvotes;
  // assume the input format is as expected.
  while(std::getline(ss_buf, one_num_str, ' ')) {
    int one_num;
    std::istringstream(one_num_str) >> one_num;
    upvotes.push_back(one_num);
  }
  return upvotes;
}

int calculate_difference(const std::vector<int>& upvotes, int k, int pos)
{
  int acc = 0;
  int prev = upvotes[pos];
  int inc_cnt = 0;
  int dec_cnt = 0;
  for(int i=pos+1; i<pos+k; ++i) {
    int next = upvotes[i];
    if(next >= prev) ++inc_cnt;
    else {
      acc += inc_cnt * (inc_cnt + 1) / 2;
      inc_cnt = 0;
    }
    if(next <= prev) ++dec_cnt;
    else {
      acc -= dec_cnt * (dec_cnt + 1) / 2;
      dec_cnt = 0;
    }
    prev = next;
  }
  acc += inc_cnt * (inc_cnt + 1) / 2 - dec_cnt * (dec_cnt + 1) / 2;
  return acc;
}

