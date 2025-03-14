package com.fy.navi.hmi.setting;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;
import android.view.View;

import com.fy.navi.hmi.databinding.DialogSettingCheckBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

public class SettingCheckDialog extends BaseFullScreenDialog<DialogSettingCheckBinding> {

    private final String title;
    private final String content;
    private final String confirmText;
    private final boolean isShowCancel;

    @Override
    protected DialogSettingCheckBinding initLayout() {
        return DialogSettingCheckBinding.inflate(LayoutInflater.from(getContext()));
    }


    protected SettingCheckDialog(Context context, String title, String content, String confirmText, boolean isShowCancel, IBaseDialogClickListener observer) {
        super(context);
        this.title = title;
        this.content = content;
        this.confirmText = confirmText;
        this.isShowCancel = isShowCancel;
        mDialogClickListener = observer;
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (content.isEmpty()) {
            mViewBinding.clearContent.setText(title);
            mViewBinding.clearContent.setPadding(0,70,0,0);
            mViewBinding.clearTitle.setVisibility(View.GONE);
        } else {
            mViewBinding.clearTitle.setText(title);
            mViewBinding.clearContent.setText(content);
        }
        if (!isShowCancel) {
            mViewBinding.dialogLine.setVisibility(View.GONE);
            mViewBinding.dialogCancel.setVisibility(View.GONE);
        }
        mViewBinding.dialogCommit.setText(confirmText);
        onClick();
    }

    public void onClick() {
        mViewBinding.dialogCancel.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) mDialogClickListener.onCancelClick();
        });
        mViewBinding.dialogCommit.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) mDialogClickListener.onCommitClick();
        });
    }


    public static class Build {

        private final Context context;
        private String title;
        private String content;
        private String confirmText;
        private boolean isShowCancel = true;
        private IBaseDialogClickListener dialogObserver;

        public Build(Context context) {
            this.context = context;
        }

        public Build setTitle(String title) {
            this.title = title;
            return this;
        }

        public Build setContent(String content) {
            this.content = content;
            return this;
        }

        public Build setConfirmText(String confirmText) {
            this.confirmText = confirmText;
            return this;
        }

        public Build setShowCancel(boolean isShowCancel) {
            this.isShowCancel = isShowCancel;
            return this;
        }

        public Build setDialogObserver(IBaseDialogClickListener dialogObserver) {
            this.dialogObserver = dialogObserver;
            return this;
        }

        public SettingCheckDialog build() {
            return new SettingCheckDialog(context, title, content, confirmText, isShowCancel, dialogObserver);
        }
    }
}
