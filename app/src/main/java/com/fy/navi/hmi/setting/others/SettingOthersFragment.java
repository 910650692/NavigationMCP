package com.fy.navi.hmi.setting.others;

import android.content.Intent;
import android.text.TextUtils;
import android.view.Window;

import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingOthersBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

public class SettingOthersFragment extends BaseFragment<FragmentSettingOthersBinding, SettingOthersViewModel> {

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
        mViewModel.initView();
        initDialog();
    }

    @Override
    public void onInitData() {
        // 初始化数据
    }

    @Override
    public void onHiddenChanged(final boolean hidden) {
        mViewModel.setTotalSizeOfDirectories(mViewModel.getTotalSizeOfDirectories());
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
            if (!TextUtils.isEmpty(userName) && !TextUtils.isEmpty(url)) {
                mBinding.accountName.setText(userName);
                mBinding.accountDetail.setText(ResourceUtils.Companion.getInstance().getText(R.string.account_login_out));
                this.mUrl = url;
                ViewAdapterKt.loadImageUrl(mBinding.accountImg, url, R.mipmap.default_user_icon, R.mipmap.default_user_icon);
                setCarConnectStatus(true);
                setWeChatStatus(mViewModel.getWechatStatus());
            } else {
                this.mUrl = "";
                mBinding.accountName.setText(ResourceUtils.Companion.getInstance().getText(R.string.setting_others_account));
                mBinding.accountDetail.setText(ResourceUtils.Companion.getInstance().getText(R.string.setting_others_account_detail));
                mBinding.accountImg.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.mipmap.default_user_icon));
                setCarConnectStatus(false);
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
            mBinding.accountImg.setImageDrawable(ResourceUtils.Companion.getInstance().getDrawable(R.mipmap.default_user_icon));
            setWeChatStatus(false);
            setCarConnectStatus(false);
        });
    }

    /**
     * 更新手车互联按钮状态
     * @param status true 已绑定，false 未绑定
     */
    public void setCarConnectStatus(final boolean status) {
        if (mViewModel.getIsLogin() && status) {
            mBinding.settingOthersCarConnectText.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.white));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_connected);
            mBinding.settingOthersCarConnect.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_broadcast_select));
        } else if (mViewModel.getIsLogin() && !status){
            mBinding.settingOthersCarConnectText.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_not_connect);
            mBinding.settingOthersCarConnect.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
        } else {
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_connect_disable);
            mBinding.settingOthersCarConnectText.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference_disable));
            mBinding.settingOthersCarConnect.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_car_number_disable));
        }
    }

    /**
     * 设置微信互联状态
     * @param status true 已绑定，false 未绑定
     */
    public void setWeChatStatus(final boolean status) {
        if (mViewModel.getIsLogin() && status) {
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_bind);
            mBinding.settingOthersWechatConnect.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.white));
            mBinding.settingOthersWechat.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_broadcast_select));
        } else if (mViewModel.getIsLogin() && !status){
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_unbind);
            mBinding.settingOthersWechatConnect.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference));
            mBinding.settingOthersWechat.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
        } else {
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_disable);
            mBinding.settingOthersWechatConnect.setTextColor(
                    ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference_disable));
            mBinding.settingOthersWechat.setBackground(
                    ResourceUtils.Companion.getInstance().getDrawable(com.fy.navi.scene.R.drawable.bg_car_number_disable));
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
                        mViewModel.deleteFilesInDirectories();
                        mViewModel.setTotalSizeOfDirectories(mViewModel.getTotalSizeOfDirectories());
                    }

                }).build();

        mResetSettingDialog  = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_reset_setting_check))
                .setContent("")
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_reset_setting_immediate))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.clearAll();
                        restartApp();
                    }

                }).build();

        mLogoutAccountDialog  = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout_detail))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout_confirm))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.logoutAccount();
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
        final Intent i = requireContext().getPackageManager()
                .getLaunchIntentForPackage(requireContext().getPackageName());
        if (i != null) {
            i.addFlags(Intent.FLAG_ACTIVITY_CLEAR_TOP);
            i.addFlags(Intent.FLAG_ACTIVITY_NEW_TASK);
            startActivity(i);
            android.os.Process.killProcess(android.os.Process.myPid());
            System.exit(0);
        }
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
