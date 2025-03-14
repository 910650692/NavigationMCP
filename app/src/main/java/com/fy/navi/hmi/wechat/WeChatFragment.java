package com.fy.navi.hmi.wechat;

import android.graphics.Bitmap;
import android.view.Window;

import com.android.utils.ResourceUtils;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.BR;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.databinding.FragmentWeChatBinding;
import com.fy.navi.hmi.setting.SettingCheckDialog;

import com.fy.navi.ui.base.BaseFragment;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description 微信互联页面
 * @Author fh
 * @date 2024/12/24
 */
public class WeChatFragment extends BaseFragment<FragmentWeChatBinding, WeChatViewModel> {

    private final static String TAG = WeChatFragment.class.getSimpleName();

    private SettingCheckDialog unbindDialog;

    @Override
    public int onLayoutId() {
        return R.layout.fragment_we_chat;
    }

    @Override
    public int onInitVariableId() {
        return BR.ViewModel;
    }

    @Override
    public void onInitView() {
        mViewModel.initView();
        initDialog();
    }

    @Override
    public void onInitData() {

    }

    public void updateQRCode(Bitmap bitmap) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.chatContentImg.setImageBitmap(bitmap);
        });
    }

    public void updateTitle(boolean isLogin) {
        ThreadManager.getInstance().postUi(() -> {
            mBinding.weChatTitle.setText(isLogin ? ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_title) : ResourceUtils.Companion.getInstance().getString(R.string.account_chat_connect));
        });
    }

    private void initDialog() {

        unbindDialog  = new SettingCheckDialog.Build(getContext())
                .setTitle(ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_unbind_title))
                .setContent(ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_unbind_tip))
                .setConfirmText(ResourceUtils.Companion.getInstance().getString(R.string.chat_connect_unbind_confirm))
                .setShowCancel(false)
                .setDialogObserver(new IBaseDialogClickListener() {
                    @Override
                    public void onCommitClick() {
                        // TODO 重启应用 恢复默认设置
                    }

                }).build();
        clearBackground(unbindDialog.getWindow());
    }

    private void clearBackground(Window window) {
        if (window != null) {
            window.setDimAmount(0f);
        }
    }

    public void showUnbindDialog() {
        if (unbindDialog == null) {
            initDialog();
        }
        unbindDialog.show();
    }
}
