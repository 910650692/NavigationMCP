package com.sgm.navi.hmi.startup;

import android.content.Intent;
import android.text.TextUtils;

import androidx.core.app.ActivityCompat;

import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.file.FileUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.NaviService;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.permission.PermissionUtils;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.service.GBLCacheFilePath;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.StartService;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.logicpaket.activate.ActivatePackage;
import com.sgm.navi.service.logicpaket.activate.IActivateObserver;
import com.sgm.navi.ui.base.BaseModel;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

import java.io.File;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class StartupModel extends BaseModel<BaseStartupViewModel>
        implements PermissionUtils.PermissionsObserver, StartService.ISdkInitCallback {

    private static final String TAG = MapDefaultFinalTag.ENGINE_HMI_TAG;
    private final CommonManager commonManager;
    private final IActivateObserver mActObserver = new IActivateObserver() {
        @Override
        public void onActivating() {
            Logger.d(TAG, "onActivating...");
            ThreadManager.getInstance().postUi(() -> mViewModel.showActivatingView(true));
        }

        @Override
        public void onNetActivateFailed(final int failedCount) {
        }

        @Override
        public void onActivated() {
            ThreadManager.getInstance().postUi(() -> mViewModel.showActivatingView(false));
        }

        @Override
        public void onActivatedError(final int errCode, final String msg) {
            Logger.e(TAG, "激活出现错误");
            ThreadManager.getInstance().postUi(new Runnable() {
                @Override
                public void run() {
                    mViewModel.showActivatingView(false);
                    mViewModel.showActivateFailedDialog(msg);
                }
            });
        }
    };

    public StartupModel() {
        Logger.d(TAG, "初始化获取缓存地图的数据库");
        commonManager = CommonManager.getInstance();
        commonManager.init();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        PermissionUtils.getInstance().setPermissionsObserver(this);
        StartService.getInstance().registerSdkCallback(TAG, this);
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        StartService.getInstance().unregisterSdkCallback(this);
        //ActivatePackage.getInstance().removeActObserver(mActObserver);
    }

    @Override
    public void onPermissionsSuccess() {
        Logger.d(TAG, "权限都申请成功 开启引擎初始化");
        if (isShowStartupException()) {
            popStartupExceptionDialog();
        } else {
            startInitEngine();
        }
    }

    @Override
    public void onPermissionsFail() {
        Logger.i(TAG, "权限申请失败无法进行下一步");
        ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.permission_quest_fail));
    }

    public boolean isShowStartupException() {
        boolean isNetConnect = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        boolean isOfflineData = "1".equals(commonManager.getValueByKey(UserDataCode.SETTING_DOWNLOAD_LIST));
        boolean isCache = false;  //目前默认false
        Logger.d(TAG, "is net connect: " + isNetConnect + ", is offline data: " + isOfflineData + ", is cached: " + isCache);
        return !(isNetConnect || isOfflineData || isCache);
    }

    private boolean isCached() {
        final String[] dirPaths = {GBLCacheFilePath.BLS_LOG};
        final File[] dirs = new File[dirPaths.length];
        for (int i = 0; i < dirPaths.length; i++) {
            dirs[i] = new File(dirPaths[i]);
        }
        return FileUtils.getTotalSizeOfDirectories(dirs) > 0;
    }

    public boolean isFirstLauncher() {
        final boolean isFirstLauncher = TextUtils.isEmpty(
                commonManager.getValueByKey(UserDataCode.SETTING_FIRST_LAUNCH)
        );
        Logger.i(TAG, "isFirstLauncher:" + isFirstLauncher);
        return isFirstLauncher;
    }

    public void checkPermission() {
        Logger.i(TAG, "checkPermission");
        if (PermissionUtils.getInstance().checkoutPermission()) {
            if (isShowStartupException()) {
                popStartupExceptionDialog();
            } else {
                startInitEngine();
            }
        } else {
            PermissionUtils.getInstance().requestPermission();
        }
    }

    public void startInitEngine() {
        Logger.d(MapDefaultFinalTag.INIT_SERVICE_TAG, "start navi Service");
        Intent intent = new Intent(AppCache.getInstance().getMContext(), NaviService.class);
        ActivityCompat.startForegroundService(AppCache.getInstance().getMContext(), intent);
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN_FAIL)
    public void popStartupExceptionDialog() {
        StartupExceptionDialog startupExceptionDialog = new StartupExceptionDialog(
            StackManager.getInstance().getCurrentActivity(MapType.MAIN_SCREEN_MAIN_MAP.name()),
            new IBaseDialogClickListener() {
                @Override
                public void onNetWorkConnect() {
                    startInitEngine();
                }

                @Override
                public void onExit() {
                    StackManager.getInstance().exitApp();
                }
            });
        startupExceptionDialog.show();
    }

    /***
     * 同意协议后把标志位置为一个非空的字符
     */
    public void updateFirstLauncherFlag() {
        commonManager.insertOrReplace(UserDataCode.SETTING_FIRST_LAUNCH, "1");
    }

    @Override
    public void onSdkInitSuccess() {
        Logger.i(TAG, "onSdkInitSuccess");
        //ActivatePackage.getInstance().addActObserver(mActObserver);
        StartService.getInstance().unregisterSdkCallback(this);
        mViewModel.startMapActivity();
    }

    @Override
    public void onSdkInitFail(int initSdkResult, String msg) {
        //ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.startup_sdk_init_fail));
        // TODO: 2025/5/5 重试机制统一在NaviService中完成
    }
}
