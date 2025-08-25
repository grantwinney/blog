+++
categories = ["Ansible", "DevOps", "DigitalOcean"]
date = 2019-12-18T22:21:00Z
description = ""
draft = false
image = "https://images.unsplash.com/photo-1429497419816-9ca5cfb4571a?ixlib=rb-1.2.1&q=80&fm=jpg&crop=entropy&cs=tinysrgb&w=2000&fit=max&ixid=eyJhcHBfaWQiOjExNzczfQ"
slug = "hands-on-ansible-using-digitalocean-ubuntu-droplets"
summary = "Today I'm wrapping my head around a build tool called Ansible, used for deploying machines in a scriptable, repeatable manner. Follow along as I step through an excellent tutorial from DigitalOcean, applying what I learn to a couple DO Ubuntu VMs... the $5/mo ones - nothing fancy needed!"
tags = ["Ansible", "DevOps", "DigitalOcean"]
title = "Hands-on Ansible, using two DigitalOcean Ubuntu droplets"

+++


For the uninitiated, Docker allows you to build VMs in a predictable, repeatable manner as a series of layers called images. Automation is where it's at – if you think you'll have to deploy a box several times, your future self will thank you for scripting it out.

I only started learning about it recently, and today I'm wrapping my head around another tool for building machines called Ansible. Note that Ansible is not an alternative for Docker, but it can actually complement it. I'll post some resources later, but right now I'm just stepping through a tutorial I found on DigitalOcean. But first...


Create two basic Ubuntu VMs

Create a DigitalOcean account and spin up two Ubuntu droplets (the green "create" button in the upper-right). A bottom-tier machine runs $5/mo, so even if you play with these for the rest of the day it'll only run ya 33¢. 🤑

Normally I'd leave "SSH keys" selected for authentication, but for now you can just select "one-time password". You'll get an email for each machine with a temp password, and then you can just open a terminal, type in ssh root@111.111.111.111 using whatever IP address you're assigned, and change the password.


Install Ansible on one of them

After you've logged into both machines, follow along with this tutorial. Pick one machine to be the "controller node", where you'll install Ansible. The other machine will be the "host" that the controller node will eventually send commands to. Everything is in the tutorial.

How to Install and Configure Ansible on Ubuntu 18.04 | DigitalOceanConfiguration management systems are designed to make controlling large numbers of servers easy for administrators and operations teams. They allow you to control many different systems in an automated way from one central location. In this guide, we will discuss how to install Ansible on an Ubuntu…DigitalOceanStephen Rees-Carter


Setup the inventory (hosts file)


Create an SSH key on the controller node

You'll need to create an SSH keypair on the same machine where you installed Ansible (the controller node). Just type ssh-keygen, accept all the defaults, then use ssh-copy-id to copy the public key you just created to the other machine (the host). That allows the node controller to communicate with the host.

How to Set Up SSH Keys on Ubuntu 18.04 | DigitalOceanSSH-key-based authentication provides a more secure alternative to password-based authentication. In this tutorial we’ll learn how to set up SSH key-based authentication on an Ubuntu 18.04 installation.DigitalOceanHanif Jetha

Here's some output from my node controller, as I was running commands. I color-coded it to make it easier to understand, but basically...

 * I tried pinging the host, which failed because SSH wasn't setup yet. (red)
 * I created an SSH keypair on the controller node. (green)
 * I verified that the keypair was created, and id_rsa.pub was present. (purple)
 * I copied the public key from the node controller to the host. (orange)
 * I ran the first command again, to ping the host. Success! (blue)


Verify that you can run Ansible commands

The authors of the tutorial suggest running the following command from the controller node, just to see that you can run commands against the host(s) you setup - although the ping command above already did that.

ansible all -a "df -h" -u root


What's next?

Okay, that wasn't nearly as bad as I thought it'd be! If you were doing this in a production environment, you'd want to do way more - creating a non-root sudo user and configuring UFW to allow only the ports you need (like 22) come to mind.

Now that I've got the servers setup and communicating, I plan on going through the rest of Erika's guides. I'll save these for another day though.

How to Use Ansible to Automate Initial Server Setup on Ubuntu | DigitalOceanAnsible offers a simple architecture that doesn’t require special software to be installed on nodes. It also provides a robust set of features and built-in modules which facilitate writing automation scripts. This guide explains how to use Ansible to automate the steps contained in our Initial Serve…DigitalOceanErika HeidiConfiguration Management 101: Writing Ansible Playbooks | DigitalOceanThis tutorial will walk you through the process of creating an automated server provisioning using Ansible, a configuration management tool that provides a complete automation framework and orchestration capabilities. We will focus on the language terminology, syntax and features necessary for creat…DigitalOceanErika HeidiHow to Use Ansible: An Ansible Cheat Sheet Guide | DigitalOceanAnsible is a modern configuration management tool that facilitates the task of setting up and maintaining remote servers. This cheat sheet-style guide provides a quick reference to commands and practices commonly used when working with Ansible.DigitalOceanErika Heidi


Other Resources

I said I'd post other resources, and I don't want to break such an important promise. So.. here's the official ansible docs. I find most of the posts on DO to be of high quality, but I'm not sure anyone's written guides for other flavors of Unix. If you're not using Ubuntu, the docs have steps for quite a few other systems, so check them out.

If you have access to a Percipio account, I found the courses created by Joseph Khoury last year to be pretty easy to understand. I have access to it through my workplace, but I don't know if you can access it as an individual like Pluralsight et al.

And of course there's YouTube, a popular video streaming site that you may not have heard of, if you were frozen 15 years ago and just thawed out today.