package com.sgm.navi.hmi.utils;

import android.view.View;

import androidx.databinding.BindingAdapter;
import androidx.databinding.ObservableField;

public class BindingUtils {
    @BindingAdapter("visibility")
    public static void setVisibility(View view, ObservableField<Boolean> visibilityField) {
        Boolean isVisible = visibilityField.get();
        view.setVisibility(isVisible != null && isVisible ? View.VISIBLE : View.GONE);
    }
}
