package com.fy.navi.hmi.setting.others;

import android.graphics.drawable.Drawable;
import android.text.TextUtils;
import android.view.Window;

import com.android.utils.ResourceUtils;
import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentSettingOthersBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;
import com.fy.navi.ui.action.ViewAdapterKt;
import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

public class SettingOthersFragment extends BaseFragment<FragmentSettingOthersBinding, SettingOthersViewModel> {

    private SettingCheckDialog clearMemoryDialog;
    private SettingCheckDialog resetSettingDialog;
    private SettingCheckDialog logoutAccountDialog;
    private String url;
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

    public void updatePrivacyStatus(boolean status) {
        if (status) {
            mBinding.settingOthersPrivacyPermissionStatus.setText(R.string.setting_others_privacy_permission_yes);
        } else {
            mBinding.settingOthersPrivacyPermissionStatus.setText(R.string.setting_others_privacy_permission_no);
        }
    }

    /**
     * 获取用户头像和账号名
     * @param userName
     * @param url
     */
    public void updateUserInfo(String userName, String url) {
        ThreadManager.getInstance().postUi(() -> {
            if (!TextUtils.isEmpty(userName) && !TextUtils.isEmpty(url)) {
                mBinding.accountName.setText(userName);
                mBinding.accountDetail.setText(ResourceUtils.Companion.getInstance().getText(R.string.account_login_out));
                this.url = url;
                ViewAdapterKt.loadImageUrl(mBinding.accountImg, url, R.mipmap.default_user_icon, R.mipmap.default_user_icon);
                setCarConnectStatus(true);
                setWeChatStatus(mViewModel.getWechatStatus());
            } else {
                this.url = "";
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
            mBinding.settingOthersWechatConnect.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_others_connect_text_fail));
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_unbind);
            mBinding.settingOthersWechat.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
            mBinding.settingOthersCarConnectText.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_others_connect_text_fail));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_not_connect);
            mBinding.settingOthersCarConnect.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
        });
    }

    public void setCarConnectStatus(boolean status) {
        if (mViewModel.getIsLogin() && status) {
            mBinding.settingOthersCarConnectText.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_connected);
            mBinding.settingOthersCarConnect.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_broadcast_select));
        } else {
            mBinding.settingOthersCarConnectText.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference));
            mBinding.settingOthersCarConnectImg.setImageResource(R.drawable.img_car_not_connect);
            mBinding.settingOthersCarConnect.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
        }
    }

    public void setWeChatStatus(boolean status) {
        if (mViewModel.getIsLogin() && status) {
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_bind);
            mBinding.settingOthersWechatConnect.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.white));
            mBinding.settingOthersWechat.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_broadcast_select));
        } else {
            mBinding.settingOthersWechatImg.setImageResource(R.drawable.img_wechat_unbind);
            mBinding.settingOthersWechatConnect.setTextColor(ResourceUtils.Companion.getInstance().getColor(R.color.setting_text_preference));
            mBinding.settingOthersWechat.setBackground(ResourceUtils.Companion.getInstance().getDrawable(R.drawable.bg_setting_tab_select));
        }
    }

    private void initDialog() {
        clearMemoryDialog = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_clear_memory_check))
                .setContent("")
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.reminder_dialog_commit))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        // TODO 清除缓存
                    }

                }).build();

        resetSettingDialog  = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_reset_setting_check))
                .setContent("")
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_reset_setting_immediate))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        // TODO 重启应用 恢复默认设置
                    }

                }).build();

        logoutAccountDialog  = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout_detail))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.setting_others_account_logout_confirm))
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        mViewModel.logoutAccount();
                    }

                }).build();

        clearBackground(clearMemoryDialog.getWindow());
        clearBackground(resetSettingDialog.getWindow());
        clearBackground(logoutAccountDialog.getWindow());
    }

    private void clearBackground(Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    public String getUserName() {
        return mBinding.accountName.getText().toString();
    }

    public String getUserIcon(){
        return url;
    }

    public void showClearMemoryDialog() {
        clearMemoryDialog.show();
    }

    public void showResetSettingDialog() {
        resetSettingDialog.show();
    }

    public void showLogoutAccountDialog() {
        logoutAccountDialog.show();
    }
}
