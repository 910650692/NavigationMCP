package com.android.utils;

import android.content.ComponentName;
import android.content.Context;
import android.content.Intent;
import android.content.ServiceConnection;
import android.os.IBinder;
import android.os.RemoteException;

import com.android.utils.log.Logger;
import com.patac.sgmsystemextendservice.ISystemExtendServiceProxy;
import com.patac.sgmsystemextendservicelib.PatacSESConstants;

/**
 * @author: QiuYaWei
 * $Revision.1.0\$
 * Date: 2025/6/7
 * Description: [分屏管理类]
 */
public class SplitScreenManager {
    private static final String PACKAGE_NAME = "com.patac.sgmsystemextendservice";
    private static final String CLS_NAME = "com.patac.sgmsystemextendservice.service.SystemExtendService";
    private boolean isServiceConnect = false;
    private ISystemExtendServiceProxy mBinder;
    private Context mContext;

    /***
     * 这里不要做耗时操作
     */
    public void init(Context context) {
        mContext = context;
        bindService();
    }

    /***
     * 当前导航全屏，SR未显示
     * .导航全屏，把SR切换到1/3屏
     */
    public void switchSRToOneThirdScreen(int taskId) {
        enterSplitScreen(
                taskId,
                PatacSESConstants.SPLIT_POSITION_RIGHT,
                PatacSESConstants.SPLIT_SIZE_2,
                PatacSESConstants.SPLIT_SCREEN_SR,
                ""
        );
    }

    /**
     * 交换位置和大小，即导航切到2/3右侧，SR切到1/3左侧
     */
    public void switchPositionAndSize() {
        // 判断当前正在分屏的应用是否是SR
        boolean isSROnSplit = false;
        String[] status = getSplitScreenStatus();
        if (!ConvertUtils.isNull(status)) {
            for (String item : status) {
                if (ConvertUtils.equals(item, PatacSESConstants.SPLIT_SCREEN_SR)) {
                    isSROnSplit = true;
                }
            }
        }
        Logger.d("screen_change_used", "isSROnSplit: " , isSROnSplit);
        if (isSROnSplit) {
            switchSplitScreen(PatacSESConstants.SWITCH_TYPE_POSITION_SIZE, "");
        } else {
            replaceISplitScreen(PatacSESConstants.SPLIT_SCREEN_NAVI, PatacSESConstants.SPLIT_SCREEN_SR, PatacSESConstants.EXIT_TYPE_OUT, "");
        }
    }

    /***
     * 导航1/3屏，点击切换智能驾驶App到全屏
     */
    public void switchSRToFullScreen() {
        exitSplitScreen(PatacSESConstants.EXIT_TYPE_OUT, PatacSESConstants.SPLIT_SCREEN_NAVI);
    }

    /***
     * 导航切换到全屏
     */
    public void switchNaviToFullScreen() {
        exitSplitScreen(PatacSESConstants.EXIT_TYPE_KEEP, PatacSESConstants.SPLIT_SCREEN_NAVI);
    }

    private static final class InstanceHolder {
        private static final SplitScreenManager instance = new SplitScreenManager();
    }

    public static SplitScreenManager getInstance() {
        return InstanceHolder.instance;
    }

    private IBinder.DeathRecipient mDeathRecipient = () -> {
        isServiceConnect = false;
        bindService();
    };

    private final ServiceConnection connection = new ServiceConnection() {
        @Override
        public void onServiceConnected(ComponentName name, IBinder service) {
            Logger.i("screen_change_used", "onServiceConnected", name.getPackageName());
            mBinder = ISystemExtendServiceProxy.Stub.asInterface(service);
            isServiceConnect = !ConvertUtils.isNull(mBinder);
            try {
                service.linkToDeath(mDeathRecipient, 0);
            } catch (RemoteException exception) {
                Logger.e("screen_change_used", exception.getMessage());
            }
        }

        @Override
        public void onServiceDisconnected(ComponentName name) {
            Logger.i("screen_change_used", "onServiceDisconnected", name.getPackageName());
            isServiceConnect = false;
        }
    };

    private void bindService() {
        Logger.i("screen_change_used", "bindService", isServiceConnect);
        try {
            if (!isServiceConnect) {
                final Intent intent = new Intent();
                intent.setClassName(PACKAGE_NAME, CLS_NAME);
                if (mContext != null) {
                    isServiceConnect = mContext.bindService(intent, connection, Context.BIND_AUTO_CREATE);
                } else {
                    Logger.e("screen_change_used", "bindService failed: mContext is null!");
                    isServiceConnect = false;
                }
            }
        } catch (SecurityException exception) {
            Logger.i("screen_change_used", "bind service failed:" + exception.getMessage());
        }
    }

    private void unBindService() {
        if (isServiceConnect && !ConvertUtils.isNull(connection) && mContext != null) {
            mContext.unbindService(connection);
            mContext = null;
        }
    }

    /***
     * 进入分屏
     * 说明：由应用侧主动触发的分屏
     * @param taskId 最终分屏左侧应用的包名
     * @param position 最终左侧应用的位置，0代表左边，1代表右边
     * @param size 最终左侧应用的大小，0代表2/3，1代表1/3
     * @param secondPkg 最终右侧应用的包名
     * @param extra 预留字段，传空字符串即可
     */
    public void enterSplitScreen(int taskId, int position, int size, String secondPkg, String extra) {
        try {
            Logger.i("screen_change_used", "enterSplitScreen-start", taskId, position, size, secondPkg, secondPkg);
            if (isServiceConnect && !ConvertUtils.isNull(mBinder)) {
                try {
                    mBinder.enterISplitScreenById(taskId, position, size, secondPkg, extra);
                } catch (RemoteException e) {
                    Logger.e("screen_change_used", e.getMessage());
                }
            } else {
                Logger.e("screen_change_used", "service disconnect or mBinder is null!");
            }
        } catch (Exception e) {
            Logger.e("screen_change_used", "enterSplitScreen-failed", taskId, position, size, secondPkg, secondPkg);
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
            Logger.i("screen_change_used", "exitSplitScreen-start", type, extra);
            if (isServiceConnect && !ConvertUtils.isNull(mBinder)) {
                try {
                    mBinder.exitISplitScreen(type, extra);
                } catch (RemoteException e) {
                    Logger.e("screen_change_used", "exitSplitScreen failed", e.getMessage());
                }
            } else {
                Logger.e("screen_change_used", "exitSplitScreen failed: service not connect or mBinder is null!");
            }
        } catch (Exception e) {
            Logger.e("screen_change_used", "exitSplitScreen-failed", e.getMessage(), type, extra);
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
        Logger.i("screen_change_used", "switchISplitScreen", type, "extra", extra);
        if (!ConvertUtils.isNull(mBinder)) {
            try {
                mBinder.switchISplitScreen(type, extra);
            } catch (RemoteException e) {
                Logger.e("screen_change_used", "switchISplitScreen");
            }
        } else {
            Logger.e("screen_change_used", "switchSplitScreen failed: service not connect or mBinder is null!");
        }
    }

    /***
     * 方法作用：替换分屏
     * @param caller 调用方的定义包名
     * @param replacePackage 需要替换的定义包名
     * @param type 1：保留调用方应用，替换另一个应用
     * 2：替换调用方应用
     * PatacSESConstants.EXIT_TYPE_KEEP = 1;
     * PatacSESConstants.EXIT_TYPE_OUT = 2;
     * @param extra
     */
    public void replaceISplitScreen(String caller , String replacePackage, int type, String extra) {
        Logger.i("screen_change_used", "replaceISplitScreen");
        if (!ConvertUtils.isNull(mBinder)) {
            try {
                mBinder.replaceISplitScreen(caller , replacePackage, type, extra);
            } catch (RemoteException e) {
                Logger.e("screen_change_used", "switchISplitScreen");
            }
        } else {
            Logger.e("screen_change_used", "switchSplitScreen failed: service not connect or mBinder is null!");
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
                Logger.e("screen_change_used", "getSplitScreenStatus failed", e.getMessage());
                return null;
            }
        }
        return null;
    }

    public int getToastOffsetX() {
        if (ScreenTypeUtils.getInstance().isFullScreen()) {
            return 0;
        }
        String[] splitScreenStatus = getSplitScreenStatus();
        if (splitScreenStatus == null) {
            return 0;
        }
        if (splitScreenStatus.length != 2) {
            return 0;
        }
        String splitLeft = splitScreenStatus[0];
        String splitRight = splitScreenStatus[1];
        if (splitLeft == null || splitRight == null) {
            return 0;
        }
        boolean isLeft = PatacSESConstants.SPLIT_SCREEN_NAVI.equals(splitLeft);
        boolean isRight = PatacSESConstants.SPLIT_SCREEN_NAVI.equals(splitRight);
        int offsetOneThreeX = ScreenTypeUtils.getInstance().getToastXOneThree();
        int offsetTwoThreeX = ScreenTypeUtils.getInstance().getToastXTwoThree();
        if (!isLeft && !isRight) {
            return 0;
        } else if (isLeft) {
            return ScreenTypeUtils.getInstance().isOneThirdScreen() ? -offsetOneThreeX : -offsetTwoThreeX;
        } else {
            return ScreenTypeUtils.getInstance().isOneThirdScreen() ? offsetOneThreeX : offsetTwoThreeX;
        }
    }
}
