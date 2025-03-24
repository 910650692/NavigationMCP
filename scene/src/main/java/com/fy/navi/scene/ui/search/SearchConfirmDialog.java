package com.fy.navi.scene.ui.search;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.fy.navi.scene.databinding.LayoutConfirmDialogBinding;
import com.fy.navi.ui.dialog.BaseFullScreenDialog;
import com.fy.navi.ui.dialog.IBaseDialogClickListener;


/**
 * @author qlzou
 * @version \$Revision1.0\$
 * @version \$Revision1.0\$
 */
public class SearchConfirmDialog extends BaseFullScreenDialog<LayoutConfirmDialogBinding> {
    private String mTitle;
    private String mContent;
    private String mConfirmTitle;

    protected SearchConfirmDialog(final Context context, final String title, final String content,
                                  final String confirmTitle, final IBaseDialogClickListener observer) {
        super(context);
        this.mTitle = title;
        this.mContent = content;
        this.mConfirmTitle = confirmTitle;
        mDialogClickListener = observer;
    }

    @Override
    protected LayoutConfirmDialogBinding initLayout() {
        return LayoutConfirmDialogBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.tvContent.setText(mContent);
        mViewBinding.tvConfirm.setText(mConfirmTitle);
        onClick();
    }

    /**
     * 设置点击事件
     */
    public void onClick() {
        mViewBinding.tvCancel.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });
        mViewBinding.tvConfirm.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCommitClick();
            }
        });
    }

    /**
     * 设置标题
     * @param title 标题文本
     */
    public void setmTitle(final String title) {
        this.mTitle = title;
    }

    public void setContent(final String content) {
        this.mContent = content;
    }

    public static class Build {
        private final Context mContext;
        private String mTitle;
        private String mContent;
        private String mConfirmTitle;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
        }

        /**
         * 设置标题
         * @param title 标题文本
         * @return Build
         */
        public Build setTitle(final String title) {
            this.mTitle = title;
            return this;
        }

        /**
         * 设置确认按钮标题文本
         * @param confirmTitle  确认按钮标题文本
         * @return Build
         */
        public Build setConfirmTitle(final String confirmTitle) {
            this.mConfirmTitle = confirmTitle;
            return this;
        }

        /**
         * 设置内容
         * @param content 内容文本
         * @return Build
         */
        public Build setContent(final String content) {
            this.mContent = content;
            return this;
        }

        /**
         * 设置点击事件监听
         * @param dialogObserver 点击事件监听
         * @return Build
         */
        public Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * 构造方法
         * @return SearchConfirmDialog对象
         */
        public SearchConfirmDialog build() {
            return new SearchConfirmDialog(mContext, mTitle, mContent, mConfirmTitle, mDialogObserver);
        }
    }
}