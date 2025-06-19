package com.fy.navi.hmi.startup;

import android.app.ActivityOptions;
import android.app.Application;
import android.content.Intent;

import androidx.annotation.NonNull;

import com.android.utils.log.Logger;
import com.fy.navi.burypoint.anno.HookMethod;
import com.fy.navi.burypoint.constant.BuryConstant;
import com.fy.navi.hmi.map.MapActivity;
import com.fy.navi.hmi.permission.ReminderDialog;
import com.fy.navi.service.AppCache;
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



    public void startMapActivity() {
        Intent intent = new Intent(AppCache.getInstance().getMContext(), MapActivity.class);
        intent.setFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
        ActivityOptions options = ActivityOptions.makeBasic();
        options.setLaunchDisplayId(0);
        AppCache.getInstance().getMContext().startActivity(intent, options.toBundle());
    }

    /**
     * 展示弹窗失败
     *
     * @param msg 错误信息
     */
    public void showActivateFailedDialog(final String msg) {
        mView.showActivateFailedDialog(msg);
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
