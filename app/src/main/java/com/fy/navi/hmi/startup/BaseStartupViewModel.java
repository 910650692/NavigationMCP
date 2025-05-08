package com.fy.navi.hmi.startup;

import android.app.Application;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.hmi.permission.ReminderDialog;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.ui.base.BaseViewModel;
import com.fy.navi.ui.base.StackManager;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/1/25
 */
public class BaseStartupViewModel extends BaseViewModel<StartupActivity, StartupModel> {
    protected static final String TAG = "BaseStartupViewModel";
    /*** 查看用户是否同意过隐私协议 **/
    private boolean isFirstLauncher;

    /*---------------其他模块打开应用需要传递的参数---------------*/
    private int mIntentPage = -1; //对应界面
    private String mKeyword; //搜索关键字
    private PoiInfoEntity mEndPoint; // 路线规划目的地

    public BaseStartupViewModel(@NonNull Application application) {
        super(application);
        Logger.d(TAG, "获取是否是第一次打开应用");
        isFirstLauncher = mModel.isFirstLauncher();
    }

    @Override
    public StartupModel initModel() {
        return new StartupModel();
    }

    @Override
    public void onCreate() {
        super.onCreate();
        Logger.d(TAG, "检测隐私弹窗和网络 缓存等");
        if (mModel.isShowStartupException()) {
            popStartupExceptionDialog();
        } else {
            checkPrivacyRights();
        }
    }

    private void checkPrivacyRights() {
        if (isFirstLauncher) {
            popAgreementDialog();
        } else {
            mModel.checkPermission();
        }
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(TAG, "onDestroy");
    }

    /**
     * 打开隐私权限弹窗
     */
    public void popAgreementDialog() {
        ReminderDialog reminderDialog = new ReminderDialog(mView, new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                mModel.updateFirstLauncherFlag();
                mModel.checkPermission();
            }

            @Override
            public void onCancelClick() {
                StackManager.getInstance().exitApp();
            }
        });
        reminderDialog.show();
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN_FAIL)
    public void popStartupExceptionDialog() {
        StartupExceptionDialog startupExceptionDialog = new StartupExceptionDialog(mView, new IBaseDialogClickListener() {
            @Override
            public void onNetWorkConnect() {
                checkPrivacyRights();
            }

            @Override
            public void onExit() {
                StackManager.getInstance().exitApp();
            }
        });
        startupExceptionDialog.show();
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
            intent.putExtra(INaviConstant.PAGE_EXTRA, mIntentPage);
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

    /**
     * 显示激活加载图片
     *
     * @param show 是否显示
     */
    public void showActivatingView(final boolean show) {
        mView.showActivatingView(show);
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

    /**
     * 重试网络激活
     */
    public void netActivateRetry() {
        mModel.netActivateRetry();
    }

    /**
     * 重试弹窗
     */
    public void showFailedDialog() {
        mView.showNetActivateFailedDialog();
    }

    /**
     * 更改重试弹窗
     */
    public void showChangedFailedDialog() {
        mView.showChangedDialog();
    }
}
