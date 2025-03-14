package com.fy.navi.hmi.navi;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.fy.navi.hmi.databinding.DialogContinueNaviBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;

/**
 * @Description TODO
 * @Author lvww
 * @date 2024/12/2
 */
public class ContinueNaviDialog extends BaseFullScreenDialog<DialogContinueNaviBinding> {
    private String title;
    private String content;

    protected ContinueNaviDialog(Context context, String title, String content, IBaseDialogClickListener observer) {
        super(context);
        this.title = title;
        this.content = content;
        mDialogClickListener = observer;
    }

    @Override
    protected DialogContinueNaviBinding initLayout() {
        return DialogContinueNaviBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.stvContinueContent.setText(content);
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

        public Build setDialogObserver(IBaseDialogClickListener dialogObserver) {
            this.dialogObserver = dialogObserver;
            return this;
        }

        public ContinueNaviDialog build() {
            return new ContinueNaviDialog(context, title, content, dialogObserver);
        }
    }
}