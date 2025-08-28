---
categories:
- Ansible
- DevOps
- DigitalOcean
date: "2019-12-18T22:21:00Z"
description: ""
draft: false
cover:
  image: photo-1429497419816-9ca5cfb4571a.jpg
slug: hands-on-ansible-using-digitalocean-ubuntu-droplets
summary: Today I'm wrapping my head around a build tool called Ansible, used for deploying
  machines in a scriptable, repeatable manner. Follow along as I step through an excellent
  tutorial from DigitalOcean, applying what I learn to a couple DO Ubuntu VMs... the
  $5/mo ones - nothing fancy needed!
tags:
- Ansible
- DevOps
- DigitalOcean
title: Hands-on Ansible, using two DigitalOcean Ubuntu droplets
---
For the uninitiated, Docker allows you to build VMs in a predictable, repeatable manner as a series of layers called images. Automation is where it's at – if you think you'll have to deploy a box several times, your future self will thank you for scripting it out.

I only started learning about it recently, and today I'm wrapping my head around another tool for building machines called Ansible. Note that Ansible is not an alternative for Docker, but [it can actually complement it](https://www.ansible.com/integrations/containers/docker). I'll post some resources later, but right now I'm just stepping through a tutorial I found on DigitalOcean. But first...

## Create two basic Ubuntu VMs

[Create a DigitalOcean account](https://m.do.co/c/448f25462030) and spin up two Ubuntu droplets (the green "create" button in the upper-right). A bottom-tier machine runs $5/mo, so even if you play with these for the rest of the day it'll only run ya 33¢. 🤑

Normally I'd leave _"SSH keys"_ selected for authentication, but for now you can just select _"one-time password"_. You'll get an email for each machine with a temp password, and then you can just open a terminal, type in `ssh root@111.111.111.111` using whatever IP address you're assigned, and change the password.

![](https://grantwinney.com/content/images/2019/12/ubuntu-vm-at-do-1.png)

![](https://grantwinney.com/content/images/2019/12/ansible-sandbox.png)

## Install Ansible on one of them

After you've logged into both machines, [follow along with this tutorial](https://www.digitalocean.com/community/tutorials/how-to-install-and-configure-ansible-on-ubuntu-18-04?ref=grantwinney.com). Pick one machine to be the "controller node", where you'll install Ansible. The other machine will be the "host" that the controller node will eventually send commands to. Everything is in the tutorial.

### Setup the inventory (hosts file)

![](https://grantwinney.com/content/images/2019/12/1-ansible-hosts-setup-1.png)

After installing Ansible, I setup the /etc/ansible/hosts file...

![](https://grantwinney.com/content/images/2019/12/2-ansible-inventory.png)

... and then verified it with the ansible-inventory command

### Create an SSH key on the controller node

You'll need to create an SSH keypair on the same machine where you installed Ansible (the controller node). Just type `ssh-keygen`, accept all the defaults, then use `ssh-copy-id` to copy the public key you just created to the other machine (the host). That allows the node controller to communicate with the host.

Follow [step 1](https://www.digitalocean.com/community/tutorials/how-to-set-up-ssh-keys-on-ubuntu-1804#step-1-%E2%80%94-create-the-rsa-key-pair) and [step 2](https://www.digitalocean.com/community/tutorials/how-to-set-up-ssh-keys-on-ubuntu-1804#step-2-%E2%80%94-copy-the-public-key-to-ubuntu-server), both from [this tutorial](https://www.digitalocean.com/community/tutorials/how-to-set-up-ssh-keys-on-ubuntu-1804).

Here's some output from my node controller, as I was running commands. I color-coded it to make it easier to understand, but basically...

- I tried pinging the host, which failed because SSH wasn't setup yet. _(red)_
- I created an SSH keypair on the controller node. _(green)_
- I verified that the keypair was created, and `id_rsa.pub` was present. _(purple)_
- I copied the public key from the node controller to the host. _(orange)_
- I ran the first command again, to ping the host. Success! _(blue)_

![](https://grantwinney.com/content/images/2019/12/ansible-node-controller-setup.png)

### Verify that you can run Ansible commands

The authors of the tutorial suggest running the following command from the controller node, just to see that you can run commands against the host(s) you setup - although the `ping` command above already did that.

```
ansible all -a "df -h" -u root
```

![](https://grantwinney.com/content/images/2019/12/ansible-df-h.png)

Checking host disk usage locally, and remotely from the controller

![](https://grantwinney.com/content/images/2019/12/change-time.png)

Checking the host date from the controller, before and after changing the host timezone

![](https://grantwinney.com/content/images/2019/12/host-uptime.png)

Checking the uptime on a host machine

## What's next?

Okay, that wasn't nearly as bad as I thought it'd be! If you were doing this in a production environment, you'd want to do _way_ more - creating a non-root sudo user and configuring UFW to allow only the ports you need (like 22) come to mind.

Now that I've got the servers setup and communicating, I plan on going through the rest of [Erika's guides](https://www.digitalocean.com/community/users/erikaheidi). I'll save these for another day though.

- [How to Use Ansible to Automate Initial Server Setup on Ubuntu 18.04 | DigitalOcean](https://www.digitalocean.com/community/tutorials/how-to-use-ansible-to-automate-initial-server-setup-on-ubuntu-18-04?ref=grantwinney.com)
- [Configuration Management 101: Writing Ansible Playbooks | DigitalOcean](https://www.digitalocean.com/community/tutorials/configuration-management-101-writing-ansible-playbooks?ref=grantwinney.com)
- [How to Use Ansible: A Reference Guide | DigitalOcean](https://www.digitalocean.com/community/cheatsheets/how-to-use-ansible-cheat-sheet-guide?ref=grantwinney.com)

### Other Resources

I said I'd post other resources, and I don't want to break such an important promise. So.. here's the official [ansible docs](https://docs.ansible.com/ansible/latest/installation_guide/intro_installation.html#latest-releases-via-apt-ubuntu). I find most of the posts on DO to be of high quality, but I'm not sure anyone's written guides for other flavors of Unix. If you're not using Ubuntu, the docs have steps for quite a few other systems, so check them out.

If you have access to a [Percipio](https://www.skillsoft.com/platform-solution/percipio/) account, I found the courses created by Joseph Khoury last year to be pretty easy to understand. I have access to it through my workplace, but I don't know if you can access it as an individual like Pluralsight et al.

And of course there's YouTube, a semi-popular video streaming site.

- [Ansible Playbook Tutorial For Beginners | Simplilearn - YouTube](https://www.youtube.com/watch?v=wgQ3rHFTM4E) *(quick overview)*
- [Ansible - A Beginner's Tutorial, Part 1 - YouTube](https://www.youtube.com/watch?v=icR-df2Olm8&embeds_referring_euri=https%3A%2F%2Fgrantwinney.com%2F) *(this is a 5-part series)*
