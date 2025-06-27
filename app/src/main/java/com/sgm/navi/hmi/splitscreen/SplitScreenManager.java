package com.sgm.navi.hmi.splitscreen;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.content.res.Configuration;
import android.os.IBinder;
import android.os.RemoteException;
import android.util.Log;

import com.android.utils.ConvertUtils;
import com.android.utils.ScreenUtils;
import com.android.utils.gson.GsonUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.hmi.utils.ScreenTypeUtils;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.BuildConfig;
import com.sgm.navi.service.define.screen.ScreenType;
import com.patac.sgmsystemextendservice.ISystemExtendServiceProxy;
import com.patac.sgmsystemextendservicelib.PatacSESConstants;
import com.sgm.navi.service.logicpaket.map.MapPackage;

import java.util.concurrent.CopyOnWriteArrayList;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/6/7
 * Description: [分屏管理类]
 */
public class SplitScreenManager {
    private static final String TAG = "SplitScreenManager";
    private static final String PACKAGE_NAME = "com.patac.sgmsystemextendservice";
    private static final String CLS_NAME = "com.patac.sgmsystemextendservice.service.SystemExtendService";
    //   ND全屏
    private final String FULL_SCREEN_JSON_PATH = BuildConfig.MAP_SDK + "/nd_maparea.json";
    //   2/3屏幕
    private String TWO_THIRD_JSON_PATH = BuildConfig.MAP_SDK + "/nd_2_3_maparea.json";
    //   1/3屏幕
    private final String ONE_THIRD_JSON_PATH = BuildConfig.MAP_SDK + "/nd_1_3_maparea.json";
    // 当前json资源
    private String currentJsonPath = FULL_SCREEN_JSON_PATH;
    private boolean isServiceConnect = false;
    private ISystemExtendServiceProxy mBinder;
    // 是否处于分屏当中
    private boolean mIsInMultiWindowMode = false;
    private final int SCREEN_FULL_WIDTH; // 单位dp,屏幕真实宽度
    private int mScreenDp;//单位dp, 屏幕可用宽度
    private final CopyOnWriteArrayList<OnScreenModeChangedListener> mListeners = new CopyOnWriteArrayList<>();
    private final int ONE_THIRD_WIDTH, TWO_THIRD_WIDTH;// 1/3, 2/3的阈值
    private ScreenType screeTypeByUseDp;
    private final int OFFSET = 50;//误差偏移量

    private SplitScreenManager() {
        SCREEN_FULL_WIDTH = ScreenUtils.Companion.getInstance().px2dp(
                ScreenUtils.Companion.getInstance().getRealScreenWidth(AppCache.getInstance().getMContext())
        );
        mScreenDp = SCREEN_FULL_WIDTH;
        ONE_THIRD_WIDTH = SCREEN_FULL_WIDTH / 3;
        TWO_THIRD_WIDTH = SCREEN_FULL_WIDTH * 2 / 3;
        Logger.i(TAG, "SplitScreenManager-Construct", "SCREEN_FULL_WIDTH:" + SCREEN_FULL_WIDTH, "ONE_THIRD_WIDTH:" + ONE_THIRD_WIDTH, "TWO_THIRD_WIDTH:" + TWO_THIRD_WIDTH);
    }

    public void registerListener(final OnScreenModeChangedListener listener, String tag) {
        if (!ConvertUtils.isNull(mListeners) && !mListeners.contains(listener)) {
            mListeners.add(listener);
            Logger.i(TAG, tag, "registerListener success!");
        } else {
            Logger.i(TAG, tag, "registerListener failed!");
        }
    }

    public void unRegisterListener(final OnScreenModeChangedListener listener, String tag) {
        Logger.i(TAG, "unRegisterListener",  tag);
        if (!ConvertUtils.isNull(mListeners)) {
            boolean result = mListeners.remove(listener);
            Logger.i(TAG, "unRegisterListener", tag, "result:" + result);
        }
    }

    /***
     * 这里不要做耗时操作
     */
    public void init() {
        bindService();
    }

    /***
     * 分屏回调
     * @param isInMultiWindowMode
     */
    public void onMultiWindowModeChanged(boolean isInMultiWindowMode) {
        Logger.i(TAG, "onMultiWindowModeChanged", isInMultiWindowMode, mIsInMultiWindowMode);
        this.mIsInMultiWindowMode = isInMultiWindowMode;
    }

    public void onConfigurationChanged() {
        Logger.i(TAG, "onConfigurationChanged");
        final Configuration configuration = AppCache.getInstance().getMContext().getResources().getConfiguration();
        Logger.d(TAG, "onConfigurationChanged-delay", GsonUtils.toJson(configuration));
        mScreenDp = configuration.screenWidthDp;
        screeTypeByUseDp = getScreeTypeByUseDp();
        if (ScreenTypeUtils.getScreenType() != screeTypeByUseDp) {
            ScreenTypeUtils.setScreenType(screeTypeByUseDp);
            notifyOnScreenSizeChanged();
        } else {
            Logger.w(TAG, "onConfigurationChanged, but not need update!");
        }
    }

    /***
     * 当前导航全屏，SR未显示
     * .导航全屏，把SR切换到1/3屏
     */
    public void switchSRToOneThirdScreen() {
        enterSplitScreen(
                PatacSESConstants.SPLIT_SCREEN_NAVI,
                PatacSESConstants.SPLIT_POSITION_RIGHT,
                PatacSESConstants.SPLIT_SIZE_2,
                PatacSESConstants.SPLIT_SCREEN_SR,
                ""
        );
    }

    /***
     * 导航1/3屏，点击切换智能驾驶App到全屏
     */
    public void switchSRToFullScreen() {
        exitSplitScreen(1, "");
    }

    /***
     * 当前位置：SR处于2/3右侧，导航左侧1/3
     * 导航切换到全屏
     */
    public void switchNaviToFullScreen() {
        Logger.i(TAG, "导航切换到全屏！");
        exitSplitScreen(0, "");
    }

    private static final class InstanceHolder {
        private static final SplitScreenManager instance = new SplitScreenManager();
    }

    public static SplitScreenManager getInstance() {
        return InstanceHolder.instance;
    }

    private IBinder.DeathRecipient mDeathRecipient = () -> {
        Log.i(TAG, "binderDied()");
        isServiceConnect = false;
        bindService();
    };

    private final ServiceConnection connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            Logger.i(TAG, "onServiceConnected", name.getPackageName());
            mBinder = ISystemExtendServiceProxy.Stub.asInterface(service);
            isServiceConnect = !ConvertUtils.isNull(mBinder);
            try {
                service.linkToDeath(mDeathRecipient, 0);
            } catch (RemoteException exception) {
                Logger.e(TAG, exception.getMessage());
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            Logger.i(TAG, "onServiceDisconnected", name.getPackageName());
            isServiceConnect = false;
        }
    };

    private void bindService() {
        Logger.i(TAG, "bindService", isServiceConnect);
        try {
            if (!isServiceConnect) {
                final Intent intent = new Intent();
                intent.setClassName(PACKAGE_NAME, CLS_NAME);
                isServiceConnect = AppCache.getInstance().getMContext().bindService(intent, connection, Context.BIND_AUTO_CREATE);
                Logger.i(TAG, isServiceConnect);
            }
        } catch (SecurityException exception) {
            Logger.i(TAG, "bind service failed:" + exception.getMessage());
        }
    }

    private void unBindService() {
        if (isServiceConnect && !ConvertUtils.isNull(connection)) {
            AppCache.getInstance().getMContext().unbindService(connection);
        }
    }

    /***
     * 进入分屏
     * 说明：由应用侧主动触发的分屏
     * @param pkg 最终分屏左侧应用的包名
     * @param position 最终左侧应用的位置，0代表左边，1代表右边
     * @param size 最终左侧应用的大小，0代表2/3，1代表1/3
     * @param secondPkg 最终右侧应用的包名
     * @param extra 预留字段，传空字符串即可
     */
    public void enterSplitScreen(String pkg, int position, int size, String secondPkg, String extra) {
        try {
            Logger.i(TAG, "enterSplitScreen-start", pkg, position, size, secondPkg, secondPkg);
            if (isServiceConnect && !ConvertUtils.isNull(mBinder)) {
                try {
                    mBinder.enterISplitScreen(pkg, position, size, secondPkg, extra);
                } catch (RemoteException e) {
                    Logger.e(TAG, e.getMessage());
                }
            } else {
                Logger.e(TAG, "service disconnect or mBinder is null!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "enterSplitScreen-failed", pkg, position, size, secondPkg, secondPkg);
        }
    }

    /***
     * 方法作用：退出分屏
     * 说明：基于应用侧主动触发的退出分屏，退出后保留一个原分屏应用进行全屏显示
     * @param type 退出分屏的类型 0:保留调用方的应用全屏显示 1：不保留调用方的应用，显示另一个分屏应用
     * @param extra
     */
    public void exitSplitScreen(int type, String extra) {
        try {
            Logger.i(TAG, "exitSplitScreen-start", type, extra);
            if (isServiceConnect && !ConvertUtils.isNull(mBinder)) {
                try {
                    mBinder.exitISplitScreen(type, extra);
                } catch (RemoteException e) {
                    Logger.e(TAG, "exitSplitScreen failed" , e.getMessage());
                }
            } else {
                Logger.e(TAG, "exitSplitScreen failed: service not connect or mBinder is null!");
            }
        } catch (Exception e) {
            Logger.e(TAG, "exitSplitScreen-failed", e.getMessage(),  type, extra);
        }
    }

    /***
     * 方法作用：切换分屏
     * 说明：
     * type 切换分屏的类型,
     * 1:代表位置变，大小不变
     * 2:代表位置变，大小变
     * 3:代表位置不变，大小变
     * @param type
     * @param extra
     */
    public void switchSplitScreen(int type, String extra) {
        Logger.i(TAG, "switchISplitScreen" , type, "extra" , extra);
        if (!ConvertUtils.isNull(mBinder)) {
            try {
                mBinder.switchISplitScreen(type, extra);
            } catch (RemoteException e) {
                Logger.e(TAG, "switchISplitScreen");
            }
        } else {
            Logger.e(TAG, "switchSplitScreen failed: service not connect or mBinder is null!");
        }
    }

    /***
     *方法作用：应用侧主动获取当前处于分屏状态的应用是谁
     * 说明：第一个数值为左侧应用，第二个数值为右侧应用
     * 示例：左导航，右SR ， (navi, sr)
     * @return
     */
    public String[] getSplitScreenStatus() {
        if (!ConvertUtils.isNull(mBinder)) {
            try {
                final String[] status = mBinder.getISplitScreenStatus();
                return status;
            } catch (RemoteException e) {
                Logger.e(TAG, "getSplitScreenStatus failed" , e.getMessage());
                return null;
            }
        }
        return null;
    }

    public void setmScreenDp(int screenDp) {
        mScreenDp = screenDp;
    }

    public ScreenType getScreeTypeByUseDp() {
        Logger.d(TAG, "getScreeTypeByUseDp", mScreenDp, ONE_THIRD_WIDTH, TWO_THIRD_WIDTH);
        ScreenType screenType;
        if (0 < mScreenDp && mScreenDp <= ONE_THIRD_WIDTH + OFFSET) {
            screenType = ScreenType.SCREEN_1_3;
        } else if (mScreenDp > ONE_THIRD_WIDTH && mScreenDp < TWO_THIRD_WIDTH + OFFSET) {
            screenType = ScreenType.SCREEN_2_3;
        } else {
            screenType = ScreenType.SCREEN_FULL;
        }
        Logger.d(TAG, "getScreeTypeByUseDp", screenType.name());
        return screenType;
    }

    public interface OnScreenModeChangedListener {
        void onScreenModeChanged(ScreenType screenType, String jsonPath);
    }

    private void notifyOnScreenSizeChanged() {
        Logger.i(TAG, "notifyOnScreenSizeChanged", mListeners.size());
        if (ScreenTypeUtils.getScreenType() == ScreenType.SCREEN_1_3) {
            currentJsonPath = ONE_THIRD_JSON_PATH;
        } else if (ScreenTypeUtils.getScreenType() == ScreenType.SCREEN_2_3) {
            currentJsonPath = TWO_THIRD_JSON_PATH;
        } else {
            currentJsonPath = FULL_SCREEN_JSON_PATH;
        }
        mListeners.forEach(listener -> {
            listener.onScreenModeChanged(ScreenTypeUtils.getScreenType(), currentJsonPath);
        });
    }

    /***
     * 获取是否处于分屏模式
     * @return
     */
    public boolean isInMultiWindow() {
        return mIsInMultiWindowMode && ScreenTypeUtils.getScreenType() != ScreenType.SCREEN_2_3;
    }

    public String getScreenJsonPath() {
        return currentJsonPath;
    }

    public boolean isOneThirdScreen() {
        return ScreenTypeUtils.getScreenType() == ScreenType.SCREEN_1_3;
    }
}
