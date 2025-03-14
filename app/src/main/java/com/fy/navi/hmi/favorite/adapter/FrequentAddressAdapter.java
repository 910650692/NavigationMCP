package com.fy.navi.hmi.favorite.adapter;

import android.content.Context;
import android.os.Bundle;
import android.text.InputType;
import android.view.Gravity;
import android.view.LayoutInflater;
import android.view.View;
import android.view.ViewGroup;
import android.view.inputmethod.EditorInfo;
import android.view.inputmethod.InputMethodManager;
import android.widget.EditText;
import android.widget.ImageView;
import android.widget.PopupWindow;
import android.widget.TextView;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import com.android.utils.log.Logger;
import com.android.utils.thread.ThreadManager;
import com.fy.navi.hmi.R;
import com.fy.navi.hmi.favorite.HomeCompanyFragment;
import com.fy.navi.service.AutoMapConstant;
import com.fy.navi.service.define.search.PoiInfoEntity;
import com.fy.navi.service.logicpaket.user.behavior.BehaviorPackage;
import com.fy.navi.ui.view.SkinConstraintLayout;
import com.fy.navi.ui.view.SkinEditText;
import com.fy.navi.ui.view.SkinLinearLayout;
import com.fy.navi.ui.view.SkinTextView;
import com.fy.navi.ui.view.SkinView;

import java.lang.ref.WeakReference;
import java.util.List;

/**
 * @Description 收藏页 - 常去地址条目
 * @Author fh
 * @date 2024/12/24
 */
public class FrequentAddressAdapter extends RecyclerView.Adapter<RecyclerView.ViewHolder> {

    private static final String TAG = FrequentAddressAdapter.class.getSimpleName();
    private List<PoiInfoEntity> frequentAddressList;
    private OnItemClickListener itemClickListener;
    private static final int TYPE_TEXT = 0;
    private static final int TYPE_IMAGE = 1;
    private final Context context;
    private int selectedPosition = -1; // -1表示无选中项

    private int currentEditPosition = -1;
    private int currentPosition = -1;
    private WeakReference<SkinEditText> focusedEditTextRef;

    private PopupWindow popupWindow;
    private TextViewHolder textViewHolder;

    public FrequentAddressAdapter(Context context) {
        this.context = context;
    }

    public void setData(List<PoiInfoEntity> list) {
        frequentAddressList = list;
        initPopupWindow();
        notifyDataSetChanged();
    }

    public void setItemClickListener(OnItemClickListener itemClickListener) {
        this.itemClickListener = itemClickListener;
    }

    @Override
    public RecyclerView.ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        if (viewType == TYPE_TEXT) {
            View view = LayoutInflater.from(context).inflate(R.layout.item_frequent_address, parent, false);
            return new TextViewHolder(view);
        } else if (viewType == TYPE_IMAGE) {
            View view = LayoutInflater.from(context).inflate(R.layout.item_add_frequent_address, parent, false);
            return new ImageViewHolder(view);
        }
        return null; // 这行代码实际上不会被执行，因为上面已经覆盖了所有可能的viewType
    }

    @Override
    public int getItemCount() {
        if (frequentAddressList == null) {
            return 1;
        }
        return frequentAddressList.size() + 1;// 加1是为了包含最后的图片item
    }
    private void showOrHideSoftInput(boolean show, TextViewHolder holder) {
        InputMethodManager imm = (InputMethodManager) context.getSystemService(Context.INPUT_METHOD_SERVICE);
        if (show) {
            imm.showSoftInput(holder.frequentAddressTextView, InputMethodManager.SHOW_IMPLICIT);
        } else {
            imm.hideSoftInputFromWindow(holder.frequentAddressTextView.getWindowToken(), 0);
        }
    }


    @Override
    public void onBindViewHolder(@NonNull RecyclerView.ViewHolder holder, int position) {
        int itemType = getItemViewType(position);
        if (itemType == TYPE_TEXT) {
            textViewHolder = (TextViewHolder) holder;
            PoiInfoEntity poiInfoEntity = frequentAddressList.get(position);
            if (poiInfoEntity.getFavoriteInfo().getCustom_name() != null && !poiInfoEntity.getFavoriteInfo().getCustom_name().equals(poiInfoEntity.getName())) {
                textViewHolder.frequentAddressTextView.setText(poiInfoEntity.getFavoriteInfo().getCustom_name());
            } else {
                textViewHolder.frequentAddressTextView.setText(poiInfoEntity.getName());
            }
            textViewHolder.frequentAddressTextView.setFocusable(false);
            textViewHolder.frequentAddressTextView.setOnEditorActionListener((v, actionId, event) -> {
                if (actionId == EditorInfo.IME_ACTION_DONE) {
                    frequentAddressList.get(position).setName(textViewHolder.frequentAddressTextView.getText().toString());
                    textViewHolder.frequentAddressTextView.setFocusable(false);
                    showOrHideSoftInput(false, textViewHolder);
                    BehaviorPackage.getInstance().modifyFavoriteData(frequentAddressList.get(position).getFavoriteInfo().getItemId(), textViewHolder.frequentAddressTextView.getText().toString());
                    return true;
                }
                return false;
            });

            textViewHolder.frequentAddressTextView.setOnClickListener(v -> {
                if (itemClickListener != null && !textViewHolder.frequentAddressTextView.isFocused()
                        && !textViewHolder.frequentAddressTextView.isFocusable()) {
                    itemClickListener.onItemClick(position);
                }
            });

            textViewHolder.frequentAddressTextView.setOnFocusChangeListener((v, hasFocus) -> {
                if (hasFocus) {
                    currentEditPosition = position;
                    focusedEditTextRef = new WeakReference<>((SkinEditText) v);
                    showOrHideSoftInput(true, textViewHolder);
                } else if (currentEditPosition == position){
                    currentEditPosition = -1;
                    focusedEditTextRef = null;
                }
            });

            textViewHolder.frequentAddressMore.setOnClickListener(v -> {
                updatePreviousItem(textViewHolder);
                popupWindow.dismiss();
                showPopupWindow(textViewHolder.frequentAddressMore, position);
            });

        } else if (itemType == TYPE_IMAGE) {
            // 不需要设置数据，因为图片是固定的
            holder.itemView.setOnClickListener(v -> {
                if (itemClickListener != null) {
                    itemClickListener.onItemAddClick(position);
                }
            });
        }
    }


    public void clearPreviousFocus() {
        // 确保有需要处理的焦点
        if (currentEditPosition != -1 && focusedEditTextRef != null) {
            // 通过弱引用获取EditText实例
            SkinEditText editText = focusedEditTextRef.get();

            // 双重验证：视图存在且仍持有焦点
            if (editText != null && editText.hasFocus()) {
                // 清除焦点
                editText.clearFocus();
            }
            // 状态重置
            currentEditPosition = -1;
            focusedEditTextRef = null;
        }
    }

    private void initPopupWindow() {

        // 创建PopupWindow的视图
        View popupView = LayoutInflater.from(context).inflate(R.layout.favorite_edit_popup, null);

        // 获取PopupWindow中的按钮并设置点击事件
        SkinTextView btnEdit = popupView.findViewById(R.id.favorite_item_rename);
        SkinTextView btnDelete = popupView.findViewById(R.id.favorite_item_delete);

        // 创建PopupWindow
        popupWindow = new PopupWindow(popupView, 300, 120,true);
        popupWindow.setContentView(popupView);
        popupWindow.setBackgroundDrawable(null); // 使PopupWindow背景透明


        btnEdit.setOnClickListener(v -> {
            popupWindow.dismiss();

            textViewHolder.frequentAddressTextView.setImeOptions(EditorInfo.IME_ACTION_DONE);
            textViewHolder.frequentAddressTextView.setInputType(InputType.TYPE_CLASS_TEXT | InputType.TYPE_TEXT_VARIATION_NORMAL);
            textViewHolder.frequentAddressTextView.setFocusableInTouchMode(true);
            textViewHolder.frequentAddressTextView.setFocusable(true);
            textViewHolder.frequentAddressTextView.requestFocus();
            String text = textViewHolder.frequentAddressTextView.getText().toString();
            textViewHolder.frequentAddressTextView.setSelection(text.length());
        });

        btnDelete.setOnClickListener(v -> {
            if (itemClickListener != null) {
                itemClickListener.onItemDeleteClick(currentPosition);
            }
            popupWindow.dismiss();
        });


    }

    public void showPopupWindow(View anchorView ,int position) {
        // 显示PopupWindow在更多按钮的上方
        currentPosition = position;
        int[] location = new int[2];
        anchorView.getLocationOnScreen(location);
        int size = frequentAddressList.size();
        int x = location[0] - 200;
        int y = location[1] - popupWindow.getHeight(); // 计算PopupWindow的Y坐标
        Logger.d(TAG, "showPopupWindow size = " + size + ",currentPosition = " + position);
        switch (size) {
            case 2 :
                if (position == 0) {
                    x = x - 590;
                }
                break;
            case 3 :
                if (position == 0) {
                    y = y - 150;
                } else if (position == 1) {
                    x = x + 590;
                    y = y - 150;
                }
                break;
            default:
                break;
        }

        popupWindow.showAtLocation(anchorView, Gravity.NO_GRAVITY, x, y);
    }


    private void updatePreviousItem(TextViewHolder holder) {

        // 先清除前一个焦点
        clearPreviousFocus();

        int previousSelected = selectedPosition;
        selectedPosition = holder.getAdapterPosition();

        // 取消之前选中的Item
        if (previousSelected != -1 && previousSelected != holder.getAdapterPosition()) {
            notifyItemChanged(previousSelected);
        }
    }

    @Override
    public int getItemViewType(int position) {
        if (position == getItemCount() - 1) {
            return TYPE_IMAGE;
        } else {
            return TYPE_TEXT;
        }
    }

    public static class TextViewHolder extends RecyclerView.ViewHolder {
        SkinEditText frequentAddressTextView;
        ImageView frequentAddressMore;

        public TextViewHolder(@NonNull View itemView) {
            super(itemView);
            frequentAddressTextView = itemView.findViewById(R.id.tv_frequent_address_title);
            frequentAddressMore= itemView.findViewById(R.id.tv_frequent_address_more);
        }
    }

    public static class ImageViewHolder extends RecyclerView.ViewHolder {
        ImageView addView;

        public ImageViewHolder(@NonNull View itemView) {
            super(itemView);
            addView = itemView.findViewById(R.id.iv_add1);
        }
    }

    public interface OnItemClickListener {
        void onItemClick(int index);
        void onItemDeleteClick(int index);
        void onItemAddClick(int index);
    }
}
