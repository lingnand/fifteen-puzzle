ifndef TARGET
$(error TARGET is not set; should be one of [mac, android])
endif

.PHONY: configure build debug clean newbuild

ifeq ($(TARGET),android)

newbuild: clean configure build

# if inside the nixos container
ifneq ("$(wildcard /nix/store)","")

configure:
	cabal configure --with-ghc=arm-unknown-linux-androideabi-ghc --with-ld=arm-linux-androideabi-ld.gold --with-ghc-pkg=arm-unknown-linux-androideabi-ghc-pkg -f target-android && \
	cp -rv dist /target/
build:
	cabal -v build && \
	cp -rv dist /target/ && \
	mv -v dist/build/fifteen-puzzle/fifteen-puzzle proj.android-studio/app/jni/libfifteen_puzzle.so && \
	ndk-build -C proj.android-studio/app NDK_MODULE_PATH=$$(eval "echo $$(jq -r '.ndk_module_path | join(":")' proj.android-studio/build-cfg.json)") NDK_TOOLCHAIN_VERSION=4.9 NDK_DEBUG=1 && \
	{ mkdir -p /target/proj.android-studio/app/libs/armeabi 2>/dev/null || true; } && \
	cp -v proj.android-studio/app/libs/armeabi/*.so /target/proj.android-studio/app/libs/armeabi/
clean:
	cabal clean

# else we should make use of the docker container
else

# need to have ANDROID_HOME set to a correct sdk location
ifndef ANDROID_HOME
$(error ANDROID_HOME is not set; must be pointing to a valid android sdk)
endif

configure:
	docker build -t nix-cross-android -f Dockerfile.nix-cross-android . && \
	docker run --rm -v `pwd`:/target nix-cross-android configure
build:
	{ mkdir -p proj.android-studio/app/assets 2>/dev/null || true; } && \
	{ if [ -n "$$(ls Resources)" ]; then rsync -av Resources/* proj.android-studio/app/assets/; fi; } && \
	docker build -t nix-cross-android -f Dockerfile.nix-cross-android . && \
	docker run --rm -v `pwd`:/target nix-cross-android build
debug: build
	cd proj.android-studio && bash ./gradlew openDebug && \
	sleep 0.5 && \
	adb shell ps | grep -m1 'org\.cocos' | awk '{ print $$2 }' | xargs adb logcat --pid
clean:
	rm -rf dist; docker rmi nix-cross-android 2>/dev/null || true

endif

else ifeq ($(TARGET),mac)

# assume on a mac
build:
	stack build --flag fifteen-puzzle:target-mac --flag cocos2d-hs:target-mac 2>&1 | awk '/^fifteen-puzzle-[0-9.]+: copy\/register$$/{ stop=1 } !stop{ print } END{ exit !stop }' && \
	mv -v fifteen-puzzle.a proj.ios_mac/mac/libs/fifteen-puzzle.a
clean:
	stack clean

endif