package com.sgm.navi.scene.ui.search;

import android.content.Context;
import android.os.Bundle;
import android.view.LayoutInflater;

import com.sgm.navi.scene.databinding.LayoutQrCodeDialogBinding;
import com.sgm.navi.ui.dialog.BaseFullScreenDialog;
import com.sgm.navi.ui.dialog.IBaseDialogClickListener;


/**
 * @author qlzou
 * @version \$Revision1.0\$
 */
public class ChargeQrCodeDialog extends BaseFullScreenDialog<LayoutQrCodeDialogBinding> {
    private String mContent;

    protected ChargeQrCodeDialog(final Context context, final String content,
                                 final IBaseDialogClickListener observer) {
        super(context);
        this.mContent = content;
        mDialogClickListener = observer;
    }

    @Override
    protected LayoutQrCodeDialogBinding initLayout() {
        return LayoutQrCodeDialogBinding.inflate(LayoutInflater.from(getContext()));
    }

    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        mViewBinding.tvContent.setText(mContent);
        onClick();
    }

    /**
     * 设置点击事件
     */
    public void onClick() {
        mViewBinding.cancel.setOnClickListener(v -> {
            dismiss();
            if (null != mDialogClickListener) {
                mDialogClickListener.onCancelClick();
            }
        });
    }


    public void setContent(final String content) {
        this.mContent = content;
    }

    public static class Build {
        private final Context mContext;
        private String mContent;
        private IBaseDialogClickListener mDialogObserver;

        public Build(final Context context) {
            this.mContext = context;
        }

        /**
         * 设置内容
         *
         * @param content 内容文本
         * @return Build
         */
        public Build setContent(final String content) {
            this.mContent = content;
            return this;
        }

        /**
         * 设置点击事件监听
         *
         * @param dialogObserver 点击事件监听
         * @return Build
         */
        public Build setDialogObserver(final IBaseDialogClickListener dialogObserver) {
            this.mDialogObserver = dialogObserver;
            return this;
        }

        /**
         * 构造方法
         *
         * @return SearchConfirmDialog对象
         */
        public ChargeQrCodeDialog build() {
            return new ChargeQrCodeDialog(mContext, mContent, mDialogObserver);
        }
    }
}