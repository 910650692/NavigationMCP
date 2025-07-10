package com.sgm.navi.hmi.setting.others;


import android.app.AlarmManager;
import android.app.PendingIntent;
import android.content.Context;
import android.content.Intent;
import android.text.TextUtils;
import android.view.Window;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.sgm.navi.burypoint.anno.HookMethod;
import com.sgm.navi.burypoint.constant.BuryConstant;
import com.sgm.navi.hmi.BR;
import com.sgm.navi.hmi.R;
import com.sgm.navi.hmi.databinding.FragmentSettingOthersBinding;
import com.sgm.navi.hmi.setting.SettingCheckDialog;
import com.sgm.navi.service.define.code.UserDataCode;
import com.sgm.navi.service.greendao.CommonManager;
import com.sgm.navi.service.logicpaket.setting.SettingPackage;
import com.sgm.navi.ui.action.ViewAdapterKt;
import com.sgm.navi.ui.base.BaseFragment;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

public class SettingOthersFragment extends BaseFragment<FragmentSettingOthersBinding, SettingOthersViewModel> {

    private final static String TAG = SettingOthersFragment.class.getSimpleName();
    private SettingCheckDialog mClearMemoryDialog;
    private SettingCheckDialog mResetSettingDialog;
    private SettingCheckDialog mLogoutAccountDialog;
    private String mUrl;
    @Override
    public int onLayoutId() {
        return R.layout.fragment_setting_others;
    }

    @Override
    public int onInitVariableId() {
        return BR.viewModel;
    }

    @Override
    public void onInitView() {
        Logger.d(TAG, "onInitView: ");
        mViewModel.initView();
        initDialog();
    }

    @Override
    public void onInitData() {
        // 初始化数据
    }

    @Override
    public void onReStoreFragment() {
        super.onReStoreFragment();
        restoreFragment();
    }

    @Override
    public void onDestroy() {
        super.onDestroy();
        clearDialog();
        mViewModel.onDestroy();
    }

    /**
     * 更新隐私权限状态
     * @param status true:已授权，false:未授权
     */
    public void updatePrivacyStatus(final boolean status) {
        if (status) {
            mBinding.settingOthersPrivacyPermissionStatus.setText(R.string.setting_others_privacy_permission_yes);
        } else {
            mBinding.settingOthersPrivacyPermissionStatus.setText(R.string.setting_others_privacy_permission_no);
        }
    }

    /**
     * 获取用户头像和账号名
     * @param userName 用户名
     * @param url 用户头像
     */
    public void updateUserInfo(final String userName, final String url) {
        ThreadManager.getInstance().postUi(() -> {
            if (TextUtils.isEmpty(userName) && TextUtils.isEmpty(url)) {
                this.mUrl = "";
                mBinding.accountName.setText(ResourceUtils.Companion.getInstance().getText(R.string.setting_others_account));
                mBinding.accountDetail.setText(ResourceUtils.Companion.getInstance().getText(R.string.setting_others_account_detail));
                mBinding.accountImg.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_default_user_icon));
                setCarConnectStatus(false);
                setWeChatStatus(false);
            } else {
                mBinding.accountName.setText(userName);
                mBinding.accountDetail.setText(ResourceUtils.Companion.getInstance().getText(R.string.account_login_out));
                this.mUrl = url;
                if (TextUtils.isEmpty(url)) {
                    mBinding.accountImg.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_default_user_icon));
                } else {
                    if (getActivity() != null) {
                        ViewAdapterKt.loadImageUrl(mBinding.accountImg, url, R.drawable.img_default_user_icon, R.drawable.img_default_user_icon);
                    }
                }
                setCarConnectStatus(true);
                setWeChatStatus(mViewModel.getWechatStatus());
            }
        });
    }

    /**
     * 退出登录，清除用户信息，显示未登录状态
     */
    public void clearUserInfo() {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.accountName.setText(ResourceUtils.Companion.getInstance().getText(R.string.setting_others_account));
            mBinding.accountDetail.setText(ResourceUtils.Companion.getInstance().getText(R.string.setting_others_account_detail));
            mBinding.accountImg.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.img_default_user_icon));
            setWeChatStatus(false);
            setCarConnectStatus(false);
        });
    }

    /**
     * 更新手车互联按钮状态
     * @param status true 已绑定，false 未绑定
     */
    public void setCarConnectStatus(final boolean status) {
        Logger.i("SettingOthersFragment", "setCarConnectStatus status = " + status);
        if (mViewModel.getIsLogin() && status) {
            mBinding.settingOthersCarConnectText.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_connected);
            mBinding.settingOthersCarConnect.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_broadcast_select));
            mBinding.settingOthersCarConnect.setAlpha(1f);
        } else if (mViewModel.getIsLogin() && !status){
            mBinding.settingOthersCarConnectText.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_not_connect);
            mBinding.settingOthersCarConnect.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
            mBinding.settingOthersCarConnect.setAlpha(1f);
        } else {
            mBinding.settingOthersCarConnectText.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_not_connect);
            mBinding.settingOthersCarConnect.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
            mBinding.settingOthersCarConnect.setAlpha(0.5f);
        }
    }

    /**
     * 设置微信互联状态
     * @param status true 已绑定，false 未绑定
     */
    public void setWeChatStatus(final boolean status) {
        Logger.i("SettingOthersFragment", "setWeChatStatus status = " + status);
        if (mViewModel.getIsLogin() && status) {
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_bind);
            mBinding.settingOthersWechatConnect.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_white));
            mBinding.settingOthersWechat.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_broadcast_select));
            mBinding.settingOthersWechat.setAlpha(1f);
        } else if (mViewModel.getIsLogin() && !status){
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_unbind);
            mBinding.settingOthersWechatConnect.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
            mBinding.settingOthersWechat.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
            mBinding.settingOthersWechat.setAlpha(1f);
        } else {
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_unbind);
            mBinding.settingOthersWechatConnect.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_bg_tab_text_unselect));
            mBinding.settingOthersWechat.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
            mBinding.settingOthersWechat.setAlpha(0.5f);
        }
    }

    /**
     * 初始化对话框
     */
    private void initDialog() {
        mClearMemoryDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_clear_memory_check))
                .setContent("")
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.reminder_dialog_commit))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.setMemoryDialogShown(false);
                        mViewModel.deleteFilesInDirectories();
                        mViewModel.setTotalSizeOfDirectories(mViewModel.getTotalSizeOfDirectories());
                    }

                    @Override
                    public void onCancelClick() {
                        mViewModel.setMemoryDialogShown(false);
                    }
                }).build();

        mResetSettingDialog  = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_reset_setting_check))
                .setContent("")
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_reset_setting_immediate))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    @HookMethod(eventName = BuryConstant.EventName.AMAP_RETURN_DEFAULT)
                    public void onCommitClick() {
                        CommonManager.getInstance().insertOrReplace(UserDataCode.GUIDE_LOGIN_LAST_TIME, "");
                        CommonManager.getInstance().insertOrReplace(UserDataCode.SETTING_FIRST_LAUNCH, "");
                        mViewModel.clearAll();
                        mViewModel.resetSetting();
                        mViewModel.setResetSettingDialogShown(false);
                        SettingPackage.getInstance().setPrivacyStatus(false);
                        restartApp();
                    }

                    @Override
                    public void onCancelClick() {
                        mViewModel.setResetSettingDialogShown(false);
                    }
                }).build();

        mLogoutAccountDialog  = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout_detail))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout_confirm))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.setLogoutAccountDialogShown(false);
                        mViewModel.logoutAccount();
                    }

                    @Override
                    public void onCancelClick() {
                        mViewModel.setLogoutAccountDialogShown(false);
                    }
                }).build();

        clearBackground(mClearMemoryDialog.getWindow());
        clearBackground(mResetSettingDialog.getWindow());
        clearBackground(mLogoutAccountDialog.getWindow());
    }

    /**
     * 清除对话框背景色
     * @param window 窗口
     */
    private void clearBackground(final Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    /**
     * 重启应用
     */
    private void restartApp() {
        final Intent i = requireContext().getPackageManager().getLaunchIntentForPackage(requireContext().getPackageName());
        Logger.i(TAG, "restartApp: 重启应用");
        if (i != null) {
            i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            PendingIntent pendingIntent = PendingIntent.getActivity(requireContext(), 0, i, PendingIntent.FLAG_IMMUTABLE);
            AlarmManager alarmManager = (AlarmManager)requireContext().getSystemService(Context.ALARM_SERVICE);
            alarmManager.set(AlarmManager.RTC, System.currentTimeMillis() + 1000, pendingIntent); // 1秒后重启
            android.os.Process.killProcess(android.os.Process.myPid());
            System.exit(0);
        }
    }

    /**
     * 保存界面
     */
    private void restoreFragment(){
        if(mViewModel.getIsClearMemoryDialogShown()){
            mClearMemoryDialog.show();
        }
        if(mViewModel.getIsResetSettingDialogShown()){
            mResetSettingDialog.show();
        }
        if(mViewModel.getIsLogoutAccountDialogShown()){
            mLogoutAccountDialog.show();
        }
    }

    /**
     * 清除dialog
     */
    private void clearDialog(){
        if(mClearMemoryDialog.isShowing()){
            mClearMemoryDialog.dismiss();
        }
        if(mResetSettingDialog.isShowing()){
            mResetSettingDialog.dismiss();
        }
        if(mLogoutAccountDialog.isShowing()){
            mLogoutAccountDialog.dismiss();
        }
        mClearMemoryDialog = null;
        mResetSettingDialog = null;
        mLogoutAccountDialog = null;
    }

    /**
     * 获取用户名
     * @return 用户名
     */
    public String getUserName() {
        return mBinding.accountName.getText().toString();
    }

    /**
     * 获取用户头像
     * @return 头像
     */
    public String getUserIcon(){
        return mUrl;
    }

    /**
     * 显示清除内存对话框
     */
    public void showClearMemoryDialog() {
        mClearMemoryDialog.show();
    }

    /**
     * 显示重置设置对话框
     */
    public void showResetSettingDialog() {
        mResetSettingDialog.show();
    }

    /**
     * 显示退出账号对话框
     */
    public void showLogoutAccountDialog() {
        mLogoutAccountDialog.show();
    }
}
