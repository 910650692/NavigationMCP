package com.fy.navi.ui.dialog;

import android.app.AlertDialog;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.graphics.drawable.Drawable;
import android.os.Bundle;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.GravityInt;
import androidx.annotation.Nullable;
import androidx.databinding.ViewDataBinding;

import java.util.Objects;

/**
 * @Description TODO
 * @Author lww
 * @date 2025/2/24
 */
public abstract class BaseDialog<V extends ViewDataBinding> extends AlertDialog {
    private static final String TAG = BaseDialog.class.getSimpleName();
    @Nullable
    protected IBaseDialogClickListener mDialogClickListener;
    protected V mViewBinding;
    protected Drawable backgroundColor;
    /*** 点击空白处是否取消Dialog false:不结束，true:结束 **/
    protected boolean outsideCancel = false;

    protected BaseDialog(Context context) {
        super(context);
        mViewBinding = initLayout();
    }

    public void setBackgroundColor(Drawable backgroundColor) {
        this.backgroundColor = backgroundColor;
    }

    public void setDialogClickListener(IBaseDialogClickListener dialogClickListener) {
        this.mDialogClickListener = dialogClickListener;
    }

    protected abstract V initLayout();

    protected abstract void initListener();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        if (null == backgroundColor)
            backgroundColor = new ColorDrawable(Color.TRANSPARENT);
        Objects.requireNonNull(getWindow()).setBackgroundDrawable(backgroundColor);
        setContentView(mViewBinding.getRoot());
        setOnShowListener(dialog -> {
            if (null != mDialogClickListener) mDialogClickListener.onShowListener();
        });
        setOnCancelListener(dialog -> {
            if (null != mDialogClickListener) mDialogClickListener.onCancelClick();
        });
        initListener();
    }

    public void showDialog(@GravityInt int position) {
        super.show();
        setCancelable(outsideCancel);
        Window window = getWindow();
        if (null == window) return;
        window.setLayout(WindowManager.LayoutParams.WRAP_CONTENT, WindowManager.LayoutParams.WRAP_CONTENT);
        WindowManager.LayoutParams params = window.getAttributes();
        params.gravity = position;
        window.setAttributes(params);
        window.addFlags(WindowManager.LayoutParams.FLAG_NOT_TOUCH_MODAL);
        window.clearFlags(WindowManager.LayoutParams.FLAG_DIM_BEHIND);
//        window.setWindowAnimations(R.style.DialogBottomAnimation);
    }

    @Override
    public boolean isShowing() {
        return super.isShowing();
    }

    @Override
    public void hide() {
        super.hide();
    }

    @Override
    public void dismiss() {
        super.dismiss();
    }

    @Override
    public void cancel() {
        super.cancel();
    }
}
