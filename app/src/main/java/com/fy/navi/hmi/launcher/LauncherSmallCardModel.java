package com.fy.navi.hmi.launcher;

import android.text.TextUtils;


import com.android.utils.NetWorkUtils;
import com.android.utils.log.Logger;
import com.fy.navi.INaviInitListener;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.startup.PermissionUtils;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.engine.IEngineObserver;
import com.fy.navi.ui.base.BaseModel;

public class LauncherSmallCardModel extends BaseModel<BaseLauncherSmallCardViewModel> implements INaviInitListener {
    private static final String TAG = "LauncherSmallCardModel";
    private final CommonManager mCommonManager;
    private final PermissionUtils mPermissionUtils;
    private IEngineObserver mEngineObserver = new IEngineObserver() {
        @Override
        public void onInitEngineSuccess() {
            Logger.d(TAG, "引擎初始化成功，开始其余模块初始化");
            checkIsAllReady();
        }

        @Override
        public void onInitEngineFail(final int code, final String msg) {
            Logger.e(TAG, "引擎初始化失败，Error Code = " + code + "; Error Massage = " + msg);
            Logger.e(TAG, "关闭应用");
        }
    };

    public LauncherSmallCardModel() {
        mCommonManager = CommonManager.getInstance();
        mCommonManager.init();
        mPermissionUtils = PermissionUtils.getInstance();
        EnginePackage.getInstance().addEngineObserver(TAG, mEngineObserver);
        NaviService.registerAppInitListener(this);
    }

    @Override
    public void onCreate() {
        super.onCreate();
        checkEngine();
    }

    private void checkIsAllReady() {
        if (isPolicyReady() && isPermissionReady() && !isShowStartupException()) {
            if (!isServiceInitReady()) {
                LauncherManager.getInstance().startInitService();
            } else {
                mViewModel.notifyMapInit();
            }
        } else {
            Logger.w(TAG, "not ready, only show guidance page!");
        }
    }

    /***
     * 检查导航服务是否初始化完成
     * @return true 已完成
     */
    private boolean isServiceInitReady() {
        return NaviService.isMapInited;
    }

    private boolean isPermissionReady() {
        return mPermissionUtils.checkoutPermission();
    }

    /***
     * 检查用户隐私协议是否已同意
     * @return true 同意
     */
    private boolean isPolicyReady() {
        return !TextUtils.isEmpty(mCommonManager.getValueByKey(UserDataCode.SETTING_FIRST_LAUNCH));
    }

    @Override
    public void onInitFinished(boolean isSuccess) {
        Logger.i(TAG, "onInitFinished:" + isSuccess);
        mViewModel.notifyMapInit();
    }

    /***
     * 参照 {@link com.fy.navi.hmi.startup.StartupModel}
     * @return
     */
    public boolean isShowStartupException() {
        boolean isNetConnect = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        boolean isOfflineData = "1".equals(mCommonManager.getValueByKey(UserDataCode.SETTING_DOWNLOAD_LIST));
        boolean isCache = false;//TODO 调用地图缓存接口
        return !(isNetConnect || isOfflineData || isCache);
    }

    /***
     * 检查引擎
     */
    public void checkEngine() {
        if (!NaviService.isMapInited) {
            EnginePackage.getInstance().initBaseLibs();//内部失败走initFailed回调结束应用
            //ActivatePackage.getInstance().startActivate();
            EnginePackage.getInstance().initBL();
        } else {
            checkIsAllReady();
        }
    }
}