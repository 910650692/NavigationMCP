package com.fy.navi.hmi.startup;

import android.content.Intent;
import android.text.TextUtils;

import androidx.core.app.ActivityCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.NetWorkUtils;
import com.android.utils.ResourceUtils;
import com.android.utils.ToastUtils;
import com.android.utils.log.Logger;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.R;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.code.UserDataCode;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.greendao.CommonManager;
import com.fy.navi.service.logicpaket.activate.ActivatePackage;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.engine.IEngineObserver;
import com.fy.navi.ui.base.BaseModel;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class StartupModel extends BaseModel<BaseStartupViewModel>
        implements PermissionUtils.PermissionsObserver {

    private static final String TAG = "StartupModel";
    private final CommonManager commonManager;
    private IEngineObserver mEngineObserver = new IEngineObserver() {
        @Override
        public void onInitEngineSuccess() {
            Logger.d(TAG, "引擎初始化成功，开始其余模块初始化");
            Intent intent = new Intent(AppContext.getInstance().getMContext(), NaviService.class);
            intent.putExtra(NaviService.START_APPLICATION_KEY, true);
            int intentPage = mViewModel.getIntentPage();
            if (intentPage != INaviConstant.OpenIntentPage.NONE) {
                intent.putExtra(INaviConstant.PAGE_EXTRA, intentPage);
                mViewModel.setIntentPage(-1);
                String searchKeyword = mViewModel.getKeyword();
                if (!TextUtils.isEmpty(searchKeyword)) {
                    intent.putExtra(INaviConstant.SEARCH_KEYWORD_EXTRA, searchKeyword);
                }
                PoiInfoEntity endPoint = mViewModel.getEndPoint();
                if (null != endPoint) {
                    intent.putExtra(INaviConstant.ROUTE_END_POI, endPoint);
                }
            }
            ActivityCompat.startForegroundService(AppContext.getInstance().getMContext(), intent);
        }

        @Override
        public void onInitEngineFail(final int code, final String msg) {
            Logger.e(TAG, "引擎初始化失败，Error Code = " + code + "; Error Massage = " + msg);
            Logger.e(TAG, "关闭应用");
            mViewModel.finishStartUp();
        }
    };

    public StartupModel() {
        commonManager = CommonManager.getInstance();
        commonManager.init();
        PermissionUtils.getInstance().setPermissionsObserver(this);
        EnginePackage.getInstance().addEngineObserver(TAG, mEngineObserver);
    }

    @Override
    public void onCreate() {
        super.onCreate();
    }

    @Override
    public void onPermissionsSuccess() {
        mViewModel.updatePermissionStatus(true);
        if (NaviService.isMapInited) {
            mViewModel.startMapActivity();
        } else {
            startInitEngine();
        }
    }

    @Override
    public void onPermissionsFail() {
        ToastUtils.Companion.getInstance().showCustomToastView(ResourceUtils.Companion.getInstance().getString(R.string.permission_quest_fail));
    }

    public void startInitEngine() {
        Logger.i(TAG, "startInitEngine:", "isMapInited:" + (NaviService.isMapInited));
        if (NaviService.isMapInited) {
            mViewModel.startMapActivity();
        } else {
            EnginePackage.getInstance().initBaseLibs();//内部失败走initFailed回调结束应用
            //ActivatePackage.getInstance().startActivate();
            EnginePackage.getInstance().initBL();
        }
    }

    public boolean isFirstLauncher() {
        final boolean isFirstLauncher = TextUtils.isEmpty(
                commonManager.getValueByKey(UserDataCode.SETTING_FIRST_LAUNCH)
        );
        Logger.i(TAG, "isFirstLauncher:" + isFirstLauncher);
        return isFirstLauncher;
    }

    public boolean isShowStartupException() {
        boolean isNetConnect = Boolean.TRUE.equals(NetWorkUtils.Companion.getInstance().checkNetwork());
        boolean isOfflineData = "1".equals(commonManager.getValueByKey(UserDataCode.SETTING_DOWNLOAD_LIST));
        boolean isCache = false;//TODO 调用地图缓存接口
        return !(isNetConnect || isOfflineData || isCache);
    }

    /***
     * 同意协议后把标志位置为一个非空的字符
     */
    public void updateFirstLauncherFlag() {
        commonManager.insertOrReplace(UserDataCode.SETTING_FIRST_LAUNCH, "1");
    }

}
