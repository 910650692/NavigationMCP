package com.fy.navi.scene.ui.search;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.fy.navi.scene.databinding.DialogSearchConfirmBinding;
import com.fy.navi.scene.databinding.LayoutConfirmDialogBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;


/**
 * @Description 确认dialog
 * @Author qlzou
 * @Date
 * @Version 1.0.0
 */
public class SearchConfirmDialog extends BaseFullScreenDialog<LayoutConfirmDialogBinding> {
    private String title;
    private String content;
    private String confirmTitle;

    protected SearchConfirmDialog(Context context, String title, String content, String confirmTitle, IBaseDialogClickListener observer) {
        super(context);
        this.title = title;
        this.content = content;
        this.confirmTitle = confirmTitle;
        mDialogClickListener = observer;
    }

    @Override
    protected LayoutConfirmDialogBinding initLayout() {
        return LayoutConfirmDialogBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.tvContent.setText(content);
        mViewBinding.tvConfirm.setText(confirmTitle);
        onClick();
    }

    public void onClick() {
        mViewBinding.tvCancel.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) mDialogClickListener.onCancelClick();
        });
        mViewBinding.tvConfirm.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) mDialogClickListener.onCommitClick();
        });
    }

    public void setTitle(String title) {
        this.title = title;
    }

    public void setContent(String content) {
        this.content = content;
    }

    public static class Build {
        private final Context context;
        private String title;
        private String content;
        private String confirmTitle;
        private IBaseDialogClickListener dialogObserver;

        public Build(Context context) {
            this.context = context;
        }

        public Build setTitle(String title) {
            this.title = title;
            return this;
        }

        public Build setConfirmTitle(String confirmTitle) {
            this.confirmTitle = confirmTitle;
            return this;
        }

        public Build setContent(String content) {
            this.content = content;
            return this;
        }

        public Build setDialogObserver(IBaseDialogClickListener dialogObserver) {
            this.dialogObserver = dialogObserver;
            return this;
        }

        public SearchConfirmDialog build() {
            return new SearchConfirmDialog(context, title, content, confirmTitle, dialogObserver);
        }
    }
}