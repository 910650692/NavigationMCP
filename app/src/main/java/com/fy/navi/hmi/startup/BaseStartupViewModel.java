package com.fy.navi.hmi.startup;

import android.app.Application;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.INaviInitListener;
import com.fy.navi.NaviService;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/1/25
 */
public class BaseStartupViewModel extends BaseViewModel<StartupActivity, StartupModel> implements INaviInitListener {
    protected static final String TAG = "BaseStartupViewModel";
    private boolean permissionStatus;
    /*** 查看用户是否同意过隐私协议 **/
    private boolean isFirstLauncher;

    /*---------------其他模块打开应用需要传递的参数---------------*/
    private int mIntentPage = -1; //对应界面
    private String mKeyword; //搜索关键字
    private PoiInfoEntity mEndPoint; // 路线规划目的地

    public BaseStartupViewModel(@NonNull Application application) {
        super(application);
        permissionStatus = PermissionUtils.getInstance().checkoutPermission();
        isFirstLauncher = mModel.isFirstLauncher();
    }

    @Override
    public StartupModel initModel() {
        return new StartupModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        NaviService.registerAppInitListener(this);
        if (isFirstLauncher) {
            popAgreementDialog();
        } else {
            checkPermission();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(TAG, "onDestroy");
        NaviService.unRegisterAppInitListener(this);
    }

    private void checkPermission() {
        if (permissionStatus) {
            mModel.startInitEngine();
        } else {
            PermissionUtils.getInstance().requestPermission();
        }
    }

    /**
     * 打开隐私权限弹窗
     */
    public void popAgreementDialog() {
        ReminderDialog reminderDialog = new ReminderDialog(mView, new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                checkPermission();
                mModel.updateFirstLauncherFlag();
            }

            @Override
            public void onCancelClick() {
                mView.finish();
            }
        });
        reminderDialog.show();
    }

    //传递参数
    public void setExtraParams(int intentPage, String keyword, PoiInfoEntity endPoint) {
        mIntentPage = intentPage;
        mKeyword = keyword;
        mEndPoint = endPoint;
    }

    public void startMapActivity() {
        Intent intent = new Intent(AppContext.getInstance().getMContext(), MapActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        if (mIntentPage != INaviConstant.OpenIntentPage.NONE) {
            intent.putExtra(INaviConstant.PAGE_EXTRA ,mIntentPage);
            mIntentPage = -1;
            if (!TextUtils.isEmpty(mKeyword)) {
                intent.putExtra(INaviConstant.SEARCH_KEYWORD_EXTRA, mKeyword);
            }
            if (null != mEndPoint) {
                intent.putExtra(INaviConstant.ROUTE_END_POI, mEndPoint);
            }
        }
        AppContext.getInstance().getMContext().startActivity(intent);
        mView.finish();
    }

    @Override
    public void onInitFinished(boolean isSuccess) {
        Logger.w(TAG, "onInitFinished" , "isSuccess:" + isSuccess, "permissionStatus:" + permissionStatus);
        if (permissionStatus) {
            startMapActivity();
        } else {
            Logger.w(TAG, "onInitFinished but not has permissionStatus!");
        }
    }

    public void updatePermissionStatus(boolean status) {
        this.permissionStatus = status;
    }

    public int getIntentPage() {
        return mIntentPage;
    }

    public void setIntentPage(int intentPage) {
        mIntentPage = intentPage;
    }

    public String getKeyword() {
        return mKeyword;
    }

    public PoiInfoEntity getEndPoint() {
        return mEndPoint;
    }
}
