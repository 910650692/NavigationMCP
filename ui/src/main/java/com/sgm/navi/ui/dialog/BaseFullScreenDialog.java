package com.sgm.navi.ui.dialog;

import android.app.AlertDialog;
import android.content.Context;
import android.graphics.Color;
import android.graphics.drawable.ColorDrawable;
import android.os.Build;
import android.os.Bundle;
import android.view.View;
import android.view.Window;
import android.view.WindowManager;

import androidx.annotation.Nullable;
import androidx.databinding.ViewDataBinding;

import java.util.Objects;

public abstract class BaseFullScreenDialog<V extends ViewDataBinding> extends AlertDialog {
    private static final String TAG = BaseFullScreenDialog.class.getSimpleName();
    protected V mViewBinding;
    @Nullable
    protected IBaseDialogClickListener mDialogClickListener;

    protected BaseFullScreenDialog(final Context context) {
        this(context, 0);
    }

    protected BaseFullScreenDialog(Context context, int themeResId) {
        super(context, themeResId);
        mViewBinding = initLayout();
    }

    public void setDialogClickListener(final IBaseDialogClickListener dialogClickListener) {
        this.mDialogClickListener = dialogClickListener;
    }

    protected abstract V initLayout();


    @Override
    protected void onCreate(final Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        Objects.requireNonNull(getWindow()).setBackgroundDrawable(new ColorDrawable(Color.TRANSPARENT));
//        setView(mViewBinding.getRoot());
        setContentView(mViewBinding.getRoot());
    }

    @Override
    public void show() {
        super.show();
        setLayoutParameterFullScreen();
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
    public void cancel() {
        super.cancel();
    }

    @Override
    public void dismiss() {
        super.dismiss();
    }

    /**
     * 设置 Dialog 的宽度和高度，使其全屏显示
     */
    private void setLayoutParameterFullScreen() {
        final Window window = getWindow();
        if (window != null) {
            if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.LOLLIPOP) {
                // 清除可能存在的透明状态栏和导航栏标志
                window.clearFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS |
                        WindowManager.LayoutParams.FLAG_TRANSLUCENT_NAVIGATION);
                // 让内容布局延伸到状态栏和导航栏之下
                window.getDecorView().setSystemUiVisibility(View.SYSTEM_UI_FLAG_LAYOUT_FULLSCREEN |
                        View.SYSTEM_UI_FLAG_LAYOUT_HIDE_NAVIGATION |
                        View.SYSTEM_UI_FLAG_LAYOUT_STABLE);
                // 允许设置状态栏和导航栏的背景颜色
                window.addFlags(WindowManager.LayoutParams.FLAG_DRAWS_SYSTEM_BAR_BACKGROUNDS);
                // 可设置状态栏和导航栏的颜色，这里设置为透明
                window.setStatusBarColor(0x00000000);
                window.setNavigationBarColor(0x00000000);
            } else if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.KITKAT) {
                // 对于 Android 4.4 到 5.0 的系统，使用透明状态栏和导航栏
                window.addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_STATUS);
                window.addFlags(WindowManager.LayoutParams.FLAG_TRANSLUCENT_NAVIGATION);
            }
            WindowManager.LayoutParams params = window.getAttributes();
            params.dimAmount = 0.0f;
            window.setAttributes(params);
            // 将 Dialog 的宽度和高度设置为全屏
            window.setLayout(WindowManager.LayoutParams.MATCH_PARENT,
                    WindowManager.LayoutParams.MATCH_PARENT);
        }
    }
}
