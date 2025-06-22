package com.sgm.navi.hmi.navi;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.sgm.navi.hmi.databinding.DialogContinueNaviBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;

/**
 * 继续导航的弹窗
 * @author sgm
 * @version $Revision.*$
 */
public class ContinueNaviDialog extends BaseFullScreenDialog<DialogContinueNaviBinding> {
    private String mTitle;
    private String mContent;

    protected ContinueNaviDialog(final Context context, final String title, final String content,
                                 final IBaseDialogClickListener observer) {
        super(context);
        this.mTitle = title;
        this.mContent = content;
        mDialogClickListener = observer;
    }

    @Override
    protected DialogContinueNaviBinding initLayout() {
        return DialogContinueNaviBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.stvContinueContent.setText(mContent);
        onClick();
    }

    /**
     * 初始化点击事件
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

    public void setTitle(final String title) {
        this.mTitle = title;
    }

    public void setContent(final String content) {
        this.mContent = content;
    }

    public static class Build {
        private final Context mContext;
        private String mTitle;
        private String mContent;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
        }

        /**
         * 设置标题
         * @param title title
         * @return build
         */
        public Build setTitle(final String title) {
            this.mTitle = title;
            return this;
        }

        /**
         * 设置内容
         * @param content content
         * @return build
         */
        public Build setContent(final String content) {
            this.mContent = content;
            return this;
        }

        /**
         * 设置dialogObserver
         * @param dialogObserver dialogObserver
         * @return build
         */
        public Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * 创建
         * @return ContinueNaviDialog
         */
        public ContinueNaviDialog build() {
            return new ContinueNaviDialog(mContext, mTitle, mContent, mDialogObserver);
        }
    }
}