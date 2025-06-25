package com.sgm.navi.hmi.startup;

import android.Manifest;
import android.content.Intent;
import android.content.res.Configuration;
import android.os.Environment;
import android.provider.Settings;
import android.view.View;
import android.view.animation.Animation;
import android.view.animation.AnimationUtils;
import android.view.animation.LinearInterpolator;

import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.core.view.WindowCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.android.utils.process.ProcessManager;
import com.android.utils.process.ProcessStatus;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.ActivityStartupBinding;
import com.sgm.navi.hmi.launcher.FloatViewManager;
import com.sgm.navi.hmi.permission.PermissionUtils;
import com.sgm.navi.service.MapDefaultFinalTag;
import com.sgm.navi.service.define.map.MapType;
import com.sgm.navi.ui.base.BaseActivity;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/11/24
 */
public class StartupActivity extends BaseActivity<ActivityStartupBinding, StartupViewModel> {
    private Animation mRotateAnim;
    private ActivateFailedDialog mFailedDialog;

    @Override
    public void onCreateBefore() {
        mScreenId = MapType.MAIN_SCREEN_MAIN_MAP.name();
    }

    @Override
    public boolean onCreateViewBefore() {
        if (!ConvertUtils.equals(ProcessStatus.AppRunStatus.DESTROYED, ProcessManager.getAppRunStatus())) {
            Logger.d(MapDefaultFinalTag.DEFAULT_TAG, "app already in foreground");
            mViewModel.startMapActivity();
            return false;
        }
        return super.onCreateViewBefore();
    }

    @Override
    public int onLayoutId() {
        return R.layout.activity_startup;
    }

    @Override
    public int onFragmentId() {
        return R.id.fragment_container;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        WindowCompat.setDecorFitsSystemWindows(getWindow(), false);
        getWindow().setNavigationBarColor(getResources().getColor(R.color.route_charge_param_color));

        mRotateAnim = AnimationUtils.loadAnimation(this, R.anim.rotate_animation);
        mRotateAnim.setDuration(2000);
        mRotateAnim.setRepeatCount(Animation.INFINITE);
        mRotateAnim.setInterpolator(new LinearInterpolator());
        mBinding.mainImg.setVisibility(View.VISIBLE);

        mFailedDialog = new ActivateFailedDialog(this);
        mFailedDialog.setDialogClickListener(new IBaseDialogClickListener() {
            @Override
            public void onCommitClick() {
                Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "重试激活");
            }

            @Override
            public void onCancelClick() {
                Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "激活失败,手动退出应用");
            }
        });
        FloatViewManager.getInstance().hideAllCardWidgets(false);
    }

    @Override
    public void onInitData() {
        Intent intent = getIntent();
        if (intent != null && intent.getIntExtra(BuryConstant.EventName.AMAP_RETURN_DEFAULT, 0)
                == BuryConstant.EventName.AMAP_RETURN_DEFAULT_CODE) {
            sendBuryPointForReset();
        }
    }

    @HookMethod(eventName = BuryConstant.EventName.AMAP_RETURN_DEFAULT)
    private void sendBuryPointForReset() {
    }

    @Override
    protected void onDestroy() {
        if (mRotateAnim != null) {
            Logger.i("startup onDestroy");
            mRotateAnim.cancel();
            mRotateAnim = null;
        }
        if (null != mFailedDialog) {
            mFailedDialog.cancel();
            mFailedDialog = null;
        }
        PermissionUtils.getInstance().remove();
        FloatViewManager.getInstance().showAllCardWidgets();
        super.onDestroy();
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        PermissionUtils.getInstance().onRequestPermissionsResult(requestCode, permissions, grantResults);
    }

    @Override
    protected void onActivityResult(int requestCode, int resultCode, @Nullable Intent data) {
        super.onActivityResult(requestCode, resultCode, data);
        switch (requestCode) {
            case PermissionUtils.REQUEST_PERMISSION_EXTERNAL_CODE:
                Logger.i("StartupActivity", "所有文件修改权限申请结果");
                if (Environment.isExternalStorageManager())
                    PermissionUtils.getInstance().onRequestPermissionsResult(android.Manifest.permission.MANAGE_EXTERNAL_STORAGE, 0);
                else
                    PermissionUtils.getInstance().onRequestPermissionsResult(Manifest.permission.MANAGE_EXTERNAL_STORAGE, -1);
                break;
            case PermissionUtils.REQUEST_PERMISSION_OVERLAY_CODE:
                Logger.i("StartupActivity", "悬浮窗申请结果");
                if (Settings.canDrawOverlays(this))
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, 0);
                else
                    PermissionUtils.getInstance().onRequestPermissionsResult(Settings.ACTION_MANAGE_OVERLAY_PERMISSION, -1);
                break;
        }
    }

    /**
     * 控制激活动画
     *
     * @param show 是否显示
     */
    public void showActivatingView(final boolean show) {
        if (show) {
            mBinding.mainImg.setVisibility(View.GONE);
            mBinding.activatingImg.setVisibility(View.VISIBLE);
            mBinding.activatingTv.setVisibility(View.VISIBLE);
            mBinding.activatingImg.startAnimation(mRotateAnim);
        } else {
            mBinding.mainImg.setVisibility(View.VISIBLE);
            mBinding.activatingImg.setVisibility(View.GONE);
            mBinding.activatingTv.setVisibility(View.GONE);
            if (mRotateAnim != null) {
                mRotateAnim.cancel(); // 停止并重置动画
                mRotateAnim.reset();
            }
        }
    }

    /**
     * 显示激活失败弹窗
     *
     * @param msg 错误信息
     */
    public void showActivateFailedDialog(final String msg) {
        if (ConvertUtils.isEmpty(mFailedDialog) || mFailedDialog.isShowing()) {
            Logger.d(MapDefaultFinalTag.ACTIVATE_SERVICE_TAG, "dialog null or showing");
            return;
        }
        mFailedDialog.show();
    }

    @Override
    public void onConfigurationChanged(@NonNull Configuration newConfig) {
        super.onConfigurationChanged(newConfig);
        recreate();
    }
}