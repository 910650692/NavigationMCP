package com.sgm.navi.service.define.message;

import java.util.Date;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * @Description:
 * @author: chao.tang
 * @date: 2025年03月15日 16:47
 * @version $Revision.0.1$
 */

@Data
@NoArgsConstructor
@AllArgsConstructor
public class MessageCenterInfo {

    private MessageCenterType msgType; //消息类型
    private String msgOperate; //消息类型
    private int srcImg;//带图片的消息

    private String msgTitle;
    private String msgContent;
    private Date msgTime;
    private int msgLenvel;

}
