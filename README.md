noiseling -- EoX-Proof of Concept 
=================================

Use the Bootscript for starting the Application on BEAM locally 
--------------------------------------------------------------
<pre><code>$ sh start.sh</pre></code>

Port the application to Erlang on Xen by creating a Xen Guest containing the LING VM
------------------------------------------------------------------------------------
<pre><code>$ rebar ling-build-image</pre></code>

Now you have a file called vmling which is ready to run directly on the hypervisor

Be sure to configure the rebar plugin ling_builder properly:
https://github.com/maximk/ling_builder

Create an AMI for running the app with Erlang on Xen on EC2
-----------------------------------------------------------
canonical receipe: http://erlangonxen.org/blog/making-amazon-ami

Create the loopback file:
<pre><code>
$ truncate -s 10M noiseling.img && mkfs.ext2 -F noiseling.img
$ mkdir tmp && sudo mount noiseling.img tmp && mkdir tmp/boot && mkdir tmp/boot/grub
$ cp vmling tmp/boot && cp menu.lst tmp/boot/grub
$ cd tmp
$ sync
$ cd ..
$ sudo umount tmp
$ rm -R tmp
</pre></code>

Now you can upload the AMI to EC2. 
Be sure to have your ec2 command line tools set up 
http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/SettingUp_CommandLine.html
<pre><code>$ mkdir tmp
$ ec2-bundle-image -c \<cert\> -k \<private_key\>  -u \<user_id\> -i noiseling.img -d tmp/ -r x86_64
$ ec2-upload-bundle -b \<s3_bucket\> -m tmp/noiseling.img.manifest.xml -a \<access_key_id\> -s \<secret_key\>
$ ec2-register ec2-noiseling/noiseling.img.manifest.xml -n noiseling
</pre></code>

!! Be aware streaming audio chunks does not work !!

