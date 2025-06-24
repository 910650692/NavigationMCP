package com.sgm.navi.exportservice;

import android.content.Intent;
import android.os.Binder;
import android.os.IBinder;
import android.os.RemoteCallbackList;
import android.os.RemoteException;
import android.text.TextUtils;

import androidx.core.app.ActivityCompat;

import com.android.utils.log.Logger;
import com.sgm.navi.NaviService;
import com.sgm.navi.exportservice.binderimpl.NaviAutoApiBinder;
import com.sgm.navi.mapservice.IBinderPool;
import com.sgm.navi.mapservice.IBinderPoolCallback;
import com.sgm.navi.mapservice.base.BinderType;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.greendao.CommonManager;

import java.util.HashMap;

public final class BinderPool extends IBinderPool.Stub {
    private static final String TAG = BinderPool.class.getSimpleName();
    private final RemoteCallbackList<IBinderPoolCallback> mBinderPoolCallbackList = new RemoteCallbackList<>();
    private final HashMap<String, Binder> mBinderMap = new HashMap<>();
    private boolean mInCallback = false;

    private static final BinderPool INSTANCE = new BinderPool();

    public static BinderPool getInstance() {
        return INSTANCE;
    }

    private BinderPool() {
        StartService.getInstance().registerSdkCallback(TAG, sdkCallback);
    }

    @Override
    public void addBindPoolCallback(final String pckName, final IBinderPoolCallback binderPoolCallback) {
        mBinderPoolCallbackList.register(binderPoolCallback, pckName);
    }

    @Override
    public boolean getEngineInitStatus(final String pckName) {
        final int initStatus = StartService.getInstance().getSdkActivation();
        Logger.d(TAG, pckName + "getEngineInit: " + initStatus);
        return -1 != initStatus;
    }

    @Override
    public void startInitEngine(final String pkgName) {
        Logger.d(TAG, pkgName + "startInitEngine");
        CommonManager commonManager = CommonManager.getInstance();
        commonManager.init();
        final boolean isFirstLauncher = TextUtils.isEmpty(
                commonManager.getValueByKey(UserDataCode.SETTING_FIRST_LAUNCH));
        if (isFirstLauncher) {
            return;
        }
        if (null != AppCache.getInstance().getMContext()) {
            Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, "start navi Service");
            final Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
            ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
        } else {
            Logger.e(TAG, "application not created");
        }
    }

    @Override
    public IBinder queryBinder(final String packName, final String binderName) {
        Logger.d(TAG, packName + " queryBinder: " + binderName);
        return getBinder(binderName, packName);
    }

    /**
     * 根据Binder名称获取对应实现.
     *
     * @param binderCode 名称.
     * @param packName   client包名.
     * @return 对应binder实现.
     */
    private synchronized IBinder getBinder(final String binderCode, final String packName) {
        Logger.d(TAG, packName + "getBinder " + binderCode);
        Binder binder = mBinderMap.get(binderCode);
        if (binder == null) {
            if (BinderType.NAVI_AUTO_API.name().equals(binderCode)) {
                binder = new NaviAutoApiBinder();
                mBinderMap.put(binderCode, binder);
            }
        }

        return binder;
    }

    private final StartService.ISdkInitCallback sdkCallback = new StartService.ISdkInitCallback() {
        @Override
        public void onSdkInitSuccess() {
            if (mInCallback) {
                return;
            }
            try {
                mInCallback = true;
                final int count = mBinderPoolCallbackList.beginBroadcast();
                for (int i = 0; i < count; i++) {
                    final IBinderPoolCallback binderPoolCallback = mBinderPoolCallbackList.getRegisteredCallbackItem(i);
                    if (null != binderPoolCallback) {
                        binderPoolCallback.onEngineInitSuccess();
                    }
                }
            } catch (RemoteException re) {
                Logger.e(TAG, "dispatch initEngineSuccess error: " + re.getMessage());
            } finally {
                mBinderPoolCallbackList.finishBroadcast();
                mInCallback = false;
            }
        }

        @Override
        public void onSdkInitFail(int initSdkResult, String msg) {
            if (mInCallback) {
                return;
            }

            try {
                mInCallback = true;
                final int count = mBinderPoolCallbackList.beginBroadcast();
                for (int i = 0; i < count; i++) {
                    final IBinderPoolCallback binderPoolCallback = mBinderPoolCallbackList.getRegisteredCallbackItem(i);
                    if (null != binderPoolCallback) {
                        binderPoolCallback.onEngineInitFailed();
                    }
                }
            } catch (RemoteException exception) {
                Logger.e(TAG, "dispatch onInitEngineFail error: " + exception.getMessage());
            } finally {
                mBinderPoolCallbackList.finishBroadcast();
                mInCallback = false;
            }
        }
    };
}
