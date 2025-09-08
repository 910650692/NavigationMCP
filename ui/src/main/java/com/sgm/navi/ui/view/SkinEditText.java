package com.sgm.navi.ui.view;

import android.content.Context;
import android.graphics.drawable.Drawable;
import android.util.AttributeSet;
import android.view.ActionMode;
import android.view.Menu;
import android.view.MenuItem;

import androidx.annotation.Nullable;
import androidx.appcompat.widget.AppCompatEditText;
import androidx.core.content.ContextCompat;

import com.android.utils.ConvertUtils;
import com.android.utils.log.Logger;
import com.sgm.navi.ui.R;


public class SkinEditText extends AppCompatEditText {
    public SkinEditText(final Context context) {
        super(context);
        disableTextSelection();
    }

    public SkinEditText(final Context context, final AttributeSet attrs) {
        super(context, attrs);
        disableTextSelection();
    }

    public SkinEditText(final Context context, final AttributeSet attrs, final int defStyleAttr) {
        super(context, attrs, defStyleAttr);
        disableTextSelection();
    }

    private static class NoActionModeCallback implements ActionMode.Callback {
        @Override
        public boolean onCreateActionMode(ActionMode mode, Menu menu) {
            return false;
        }

        @Override
        public boolean onPrepareActionMode(ActionMode mode, Menu menu) {
            return false;
        }

        @Override
        public boolean onActionItemClicked(ActionMode mode, MenuItem item) {
            return false;
        }

        @Override
        public void onDestroyActionMode(ActionMode mode) {
            // 无需特殊处理
        }
    }
    private static final NoActionModeCallback NO_ACTION_MODE_CALLBACK = new NoActionModeCallback();
    private void disableTextSelection() {
        // 禁用水滴选择器
        setCursorVisible(true);
        setLongClickable(false);
        setTextIsSelectable(false);
        setSelectAllOnFocus(false);

        // 设置透明的文本选择手柄
        try {
            Drawable drawable = ContextCompat.getDrawable(getContext(), R.drawable.transparent_drawable);
            if (drawable != null) {
                setTextSelectHandle(drawable);
            }
        } catch (Exception e) {
            Logger.e("SkinEditText", "Failed to set transparent drawable for text select handle", e);
        }

        setCustomSelectionActionModeCallback(NO_ACTION_MODE_CALLBACK);
        setCustomInsertionActionModeCallback(NO_ACTION_MODE_CALLBACK);
    }
}
