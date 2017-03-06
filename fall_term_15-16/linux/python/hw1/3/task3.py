#! /usr/bin/env python3

import sys
import getopt
import requests
import pickle


class VKUser:
    def __init__(self, name, uid):
        self.name = name
        self.uid = uid

    def __str__(self):
        return self.name + " [ " + str(self.uid) + " ]"

    def __eq__(self, other):
        if isinstance(other, VKUser):
            return self.uid == other.uid
        else:
            return False

    def __ne__(self, other):
        return not self.__eq__(other)

    def __hash__(self):
        return self.uid


def usage():
    print("USAGE: ")
    print("      stalker.py -d[--diff] %vk_user_id")
    print("           Prints difference between last saved friends list and"
          "it's current version for VK.com user with id: %vk_user_id%")
    print("      stalker.py -s[--save] %vk_user_id")
    print("           Save on disk current friends list for VK user with "
          "given user id %vk_user_id%")


def get_friends_filename(uid):
    return 'friends_of_{}.data'.format(uid)


def get_friends_for_user(uid):
    r = requests.get("https://api.vk.com/method/friends.get",
                     params={'user_id': uid, 'fields': ['uid',
                                                        'first_name',
                                                        'last_name']})

    if r.status_code != requests.codes.ok:
        print("ERROR: can't make API call to VK to get friends for user!")
        exit(0)
    d = r.json()

    friends = [VKUser(user_dict['first_name'] + ' ' + user_dict['last_name'],
               int(user_dict['uid'])) for user_dict in d['response']]

    return set(friends)


def get_user(uid):
    r = requests.get("https://api.vk.com/method/users.get",
                     params={'uid': uid})
    if r.status_code != requests.codes.ok:
        print("ERROR: can't make API call to VK to get user data!")
        exit(0)
    d = r.json()['response'][0]
    return VKUser(d['first_name'] + ' ' + d['last_name'], int(d['uid']))


def save(uid):
    friends = get_friends_for_user(uid)
    with open(get_friends_filename(uid), 'wb') as f:
        pickle.dump(friends, f)


def diff(uid):
    cur_friends = get_friends_for_user(uid)
    try:
        f = open(get_friends_filename(uid), 'rb')
        old_friends = pickle.load(f)
        deleted_fds = old_friends.difference(cur_friends)
        added_fds = cur_friends.difference(old_friends)

        target_user = get_user(uid)

        print("=== Friend list update info for user {} ==="
              .format(target_user))
        if not added_fds:
            print("    No friends added!")
        else:
            print("    Friends added:")
            for new_friend in added_fds:
                print("        {}".format(new_friend))
        if not deleted_fds:
            print("    No friends deleted!")
        else:
            print("    Friends deleted:")
            for del_friend in deleted_fds:
                print("        {}".format(del_friend))

    except FileNotFoundError:
        print("ERROR: u can't diff before any 'save' operation!")
        exit(0)


def main():
    try:
        s_opts = "s:d:"
        l_opts = ["save=", "diff="]
        opts, args = getopt.getopt(sys.argv[1:], s_opts, l_opts)
        if not opts:
            opts, args \
                = getopt.getopt(args[::-1], s_opts, l_opts)
    except getopt.GetoptError as err:
        print(str(err))
        usage()
        sys.exit(0)

    if len(opts) != 1 or args:
        print("ERROR: bad script arguments!")
        usage()
        sys.exit(0)

    try:
        uid = int(opts[0][1])
    except ValueError:
        print("ERROR: cannot interpret [{}] as user id".format(opts[0][1]))
        usage()
        exit(0)

    if opts[0][0] in ['-s', '--save']:
        save(uid)
    else:
        diff(uid)


if __name__ == '__main__':
    main()
