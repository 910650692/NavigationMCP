package com.fy.navi.hmi.launcher;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.graphics.Bitmap;
import android.graphics.Matrix;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

import androidx.annotation.Nullable;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.fy.navi.service.AppContext;
import com.patac.launcher.ILauncherCallback;
import com.patac.launcher.ILauncherModeManager;
import com.patac.launcher.PatacLauncherModeConfig;

import java.nio.ByteBuffer;

public class FloatViewManager {
    private static final String TAG = "FloatViewManager";
    private boolean isServiceConnect = false;
    private int startX, startY;
    private ILauncherModeManager mLauncherModeManager;
    /* 回调监听左上角 导航小卡窗口是否显示*/
    private static ILauncherCallback mLauncherCallback = new ILauncherCallback.Stub() {
        @Override
        public void onNavigationCardVisible(boolean b) throws RemoteException {
            // b - true 显示； false 隐藏
            showOrHideFloatView(b);
            Logger.d(TAG, "onNavigationCardVisible b = " + b);
        }

        @Override
        public void onLauncherStateChanged(int i) throws RemoteException {
            // Nothing to do , reserved.
            Logger.d(TAG, "onLauncherStateChanged i = " + i);
        }
    };
    private IBinder.DeathRecipient mDeathRecipient = () -> {
        Log.i(TAG, "binderDied()");
        isServiceConnect = false;
        bindLauncherService();
    };
    private final ServiceConnection mConnection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName componentName, IBinder service) {
            mLauncherModeManager = ILauncherModeManager.Stub.asInterface(service);
            isServiceConnect = mLauncherModeManager != null;
            Log.i(TAG, "onServiceConnected()");
            if (mLauncherModeManager != null) {
                try {
                    mLauncherModeManager.registerLauncherCallback(mLauncherCallback);
                } catch (RemoteException e) {
                    e.printStackTrace();
                }
            }
            try {
                service.linkToDeath(mDeathRecipient, 0);
            } catch (RemoteException e) {
                e.printStackTrace();
            }

        }

        @Override
        public void onServiceDisconnected(ComponentName componentName) {
            Logger.i(TAG, "onServiceDisconnected()");
            isServiceConnect = false;
        }
    };

    private FloatViewManager() {

    }

    private static final class Holder {
        private static final FloatViewManager instance = new FloatViewManager();
    }

    public static FloatViewManager getInstance() {
        return Holder.instance;
    }

    @Nullable
    private static LauncherWindowService mWindowService;

    public void bindWindowService(LauncherWindowService service) {
        Logger.i(TAG, "bindWindowService success!");
        this.mWindowService = service;
        bindLauncherService();
    }

    public void unBindWindowService() {
        Logger.i(TAG, "unBindWindowService success!");
        this.mWindowService = null;
    }

    public static void showOrHideFloatView(boolean isShow) {
        if (!ConvertUtils.isNull(mWindowService)) {
            mWindowService.showOrHideFloatView(isShow);
        }
    }

    private void bindLauncherService() {
        if (isServiceConnect) {
            Logger.d(TAG, "bindLauncherService", "service had connected!");
            return;
        }
        Intent intent = new Intent(PatacLauncherModeConfig.ACTION);
        intent.setPackage("com.patac.launcher");
        isServiceConnect = AppContext.getInstance().getMContext().bindService(intent, mConnection, Context.BIND_AUTO_CREATE);
        Logger.i(TAG, "bindLauncherService", "bindResult:" + isServiceConnect);
    }

    public Bitmap processPicture(byte[] bytes) {
        Bitmap orginBitmap = null;
        Bitmap flippedBitmap = null;
        Bitmap cropBitmap;
        try {
            // 注意：这个宽高要和API里面设置的裁剪区域大小一致
            int width = 3950;
            int height = 1320;
            orginBitmap = Bitmap.createBitmap(width, height, Bitmap.Config.ARGB_8888);
            orginBitmap.copyPixelsFromBuffer(ByteBuffer.wrap(bytes));
            // 翻转图像
            Matrix matrix = new Matrix();
            matrix.postScale(1, -1);
            matrix.postTranslate(orginBitmap.getWidth(), orginBitmap.getHeight());
            flippedBitmap = Bitmap.createBitmap(orginBitmap, 0, 0, orginBitmap.getWidth(), orginBitmap.getHeight(), matrix, true);
            cropBitmap = Bitmap.createBitmap(flippedBitmap, 320, 320, 800, 450);
        } catch (Exception e) {
            throw new RuntimeException(e);
        } finally {
            if (!ConvertUtils.isNull(orginBitmap)) {
                orginBitmap.recycle();
            }
            if (!ConvertUtils.isNull(flippedBitmap)) {
                flippedBitmap.recycle();
            }
        }
        return cropBitmap;
    }
}
