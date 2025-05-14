package com.fy.navi.hmi.setting;

import android.content.Context;
import android.os.Bundle;
import android.text.TextUtils;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

import com.fy.navi.hmi.BuildConfig;
import com.fy.navi.hmi.databinding.DialogSettingCheckBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

public class SettingCheckDialog extends BaseFullScreenDialog<DialogSettingCheckBinding> {

    private final String mTitle;
    private final String mContent;
    private final String mConfirmText;
    private final boolean mIsShowCancel;

    public static final int TRADITIONAL = 1;
    public static final int DEFAULT = 0;

    @Override
    protected DialogSettingCheckBinding initLayout() {
        return DialogSettingCheckBinding.inflate(LayoutInflater.from(getContext()));
    }


    protected SettingCheckDialog(final Context context, final String title, final String content, final String confirmText,
                                 final boolean isShowCancel, final IBaseDialogClickListener observer) {
        super(context, TextUtils.equals("cadi", BuildConfig.FLAVOR) ? TRADITIONAL : DEFAULT);
        this.mTitle = title;
        this.mContent = content;
        this.mConfirmText = confirmText;
        this.mIsShowCancel = isShowCancel;
        mDialogClickListener = observer;
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (mContent.isEmpty()) {
            mViewBinding.clearContent.setText(mTitle);
            mViewBinding.clearContent.setPadding(0,70,0,0);
            mViewBinding.clearTitle.setVisibility(View.GONE);
        } else {
            mViewBinding.clearTitle.setText(mTitle);
            mViewBinding.clearContent.setText(mContent);
        }
        if (!mIsShowCancel) {
            mViewBinding.dialogCancel.setVisibility(View.GONE);
        }
        mViewBinding.dialogCommit.setText(mConfirmText);
        onClick();
    }

    @Override
    public void show() {
        super.show();
        final Window window = getWindow();
        if(TextUtils.equals("clea_local_8155", BuildConfig.FLAVOR)){
            if(window != null) window.setDimAmount(0.3f);
        }
        if(TextUtils.equals("cadi", BuildConfig.FLAVOR)){
            if (null == window) {
                return;
            }
            window.setLayout(WindowManager.LayoutParams.WRAP_CONTENT, WindowManager.LayoutParams.WRAP_CONTENT);
            final WindowManager.LayoutParams params = window.getAttributes();
            params.gravity = Gravity.START | Gravity.BOTTOM;
            params.x = getContext().getResources().getDimensionPixelOffset(com.fy.navi.ui.R.dimen.dp_255);
            params.y = getContext().getResources().getDimensionPixelOffset(com.fy.navi.ui.R.dimen.dp_50);
            window.setAttributes(params);
            window.addFlags(WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL);
            window.clearFlags(WindowManager.LayoutParams.FLAG_DIM_BEHIND);
        }
    }

    /**
     * 初始化监听
     */
    public void onClick() {
        mViewBinding.dialogCancel.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });
        mViewBinding.dialogCommit.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCommitClick();
            }
        });
    }


    public static class Build {

        private final Context mContext;
        private String mTitle;
        private String mContent;
        private String mConfirmText;
        private boolean mIsShowCancel = true;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
        }

        /**
         * 设置标题
         * @param title 标题
         * @return Build
         */
        public Build setTitle(final String title) {
            this.mTitle = title;
            return this;
        }

        /**
         * 设置内容
         * @param content 内容
         * @return Build
         */
        public Build setContent(final String content) {
            this.mContent = content;
            return this;
        }

        /**
         * 设置确认按钮文案
         * @param confirmText 确认按钮文案
         * @return Build
         */
        public Build setConfirmText(final String confirmText) {
            this.mConfirmText = confirmText;
            return this;
        }

        /**
         * 设置是否显示取消按钮
         * @param isShowCancel 是否显示取消按钮
         * @return Build
         */
        public Build setShowCancel(final boolean isShowCancel) {
            this.mIsShowCancel = isShowCancel;
            return this;
        }

        /**
         * 设置监听
         * @param dialogObserver 监听
         * @return Build
         */
        public Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * 创建对话框
         * @return 对话框
         */
        public SettingCheckDialog build() {
            return new SettingCheckDialog(mContext, mTitle, mContent, mConfirmText, mIsShowCancel, mDialogObserver);
        }
    }
}
