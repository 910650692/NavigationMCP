package com.fy.navi.hmi.startup;

import android.app.Application;
import android.content.Intent;
import android.text.TextUtils;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.INaviInitListener;
import com.fy.navi.NaviService;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.mapservice.bean.INaviConstant;
import com.fy.navi.service.AppContext;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.activate.ActivatePackage;
import com.fy.navi.service.logicpaket.engine.EnginePackage;
import com.fy.navi.service.logicpaket.activate.IActivateObserver;
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

    private NetActivateFailedDialog mFailedDialog;
    private IActivateObserver mActObserver = new IActivateObserver() {
        @Override
        public void onActivating() {
            Logger.d(TAG, "onActivating...");
            showActivatingView(true);
        }

        @Override
        public void onNetActivateFailed(final int failedCount) {
            Logger.d(TAG, "onNetActivateFailed count = " + failedCount);
            showActivatingView(false);
            if (failedCount >= 3) {
                mFailedDialog.setConfirmText();
                mFailedDialog.setDialogClickListener(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        Logger.d(TAG, "确认跳转手动激活");
                        addFragment(new ManualActivateFragment(), null);
                    }

                    @Override
                    public void onCancelClick() {
                        Logger.e(TAG, "网络激活失败，手动退出应用");
                        finishStartUp();
                    }
                });
            }
            mFailedDialog.show();
        }

        @Override
        public void onActivated() {
            Logger.d(TAG, "激活成功回调，开始BL初始化");
            EnginePackage.getInstance().initBL();
        }

        @Override
        public void onActivatedError() {
            Logger.e(TAG, "激活出现错误，退出应用");
            showActivatingView(false);
            finishStartUp();
        }
    };

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
        } else if (mModel.isShowStartupException()) {
            popStartupExceptionDialog();
        } else {
            checkPermission();
        }
        ActivatePackage.getInstance().addActObserver(mActObserver);
        mFailedDialog = new NetActivateFailedDialog(mView);
        mFailedDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                Logger.d(TAG, " 重试网络激活");
                ActivatePackage.getInstance().netActivateRetry();
            }

            @Override
            public void onCancelClick() {
                Logger.d(TAG, "激活失败，手动退出应用");
                finishStartUp();
            }
        });
    }

    /**
     * 关闭activity
     */
    public void finishStartUp() {
        mView.finish();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        Logger.i(TAG, "onDestroy");
        mFailedDialog.cancel();
        NaviService.unRegisterAppInitListener(this);
        ActivatePackage.getInstance().removeActObserver(mActObserver);
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
                if (mModel.isShowStartupException()) {
                    popStartupExceptionDialog();
                } else {
                    checkPermission();
                    mModel.updateFirstLauncherFlag();
                }
            }

            @Override
            public void onCancelClick() {
                finishStartUp();
            }
        });
        reminderDialog.show();
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_OPEN_FAIL)
    public void popStartupExceptionDialog() {
        StartupExceptionDialog startupExceptionDialog = new StartupExceptionDialog(mView, new IBaseDialogClickListener() {
            @Override
            public void onNetWorkConnect() {
                checkPermission();
                if (isFirstLauncher) {
                    mModel.updateFirstLauncherFlag();
                }
            }

            @Override
            public void onExit() {
                finishStartUp();
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
        finishStartUp();
    }

    @Override
    public void onInitFinished(boolean isSuccess) {
        Logger.w(TAG, "onInitFinished", "isSuccess:" + isSuccess, "permissionStatus:" + permissionStatus);
        if (!isSuccess) {
            return;
        }
        if (permissionStatus) {
            startMapActivity();
        } else {
            Logger.w(TAG, "onInitFinished but not has permissionStatus!");
        }
    }

    /**
     * 显示激活加载图片
     * @param show 是否显示
     */
    public void showActivatingView(final boolean show) {
        mView.showActivatingView(show);
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
