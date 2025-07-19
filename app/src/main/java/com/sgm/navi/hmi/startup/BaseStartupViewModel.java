package com.sgm.navi.hmi.startup;

import android.app.ActivityOptions;
import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.sgm.navi.hmi.map.MapActivity;
import com.sgm.navi.hmi.permission.ReminderDialog;
import com.sgm.navi.service.AppCache;
import com.sgm.navi.ui.base.BaseViewModel;
import com.sgm.navi.ui.base.StackManager;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/1/25
 */
public class BaseStartupViewModel extends BaseViewModel<StartupActivityRemove, StartupModel> {
    protected static final String TAG = "BaseStartupViewModel";
    /*** 查看用户是否同意过隐私协议 **/
    private boolean isFirstLauncher;

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
        checkPrivacyRights();
    }

    private void checkPrivacyRights() {
        Logger.i(TAG, "checkPrivacyRights");
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

    public void startMapActivity() {
        Intent intent = new Intent(AppCache.getInstance().getMContext(), MapActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
        if (mView != null) {
            mView.finishThisActivity();
        }
    }
    /**
     * 显示激活加载图片
     *
     * @param show 是否显示
     */
    public void showActivatingView(final boolean show) {
        mView.showActivatingView(show);
    }
}
