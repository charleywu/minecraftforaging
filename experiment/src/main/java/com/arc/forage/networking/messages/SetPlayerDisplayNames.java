package com.arc.forage.networking.messages;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.nio.charset.Charset;
import java.util.Base64;
import java.util.List;
import java.util.stream.Collectors;

import org.apache.commons.lang3.StringUtils;

import com.arc.forage.gamemaster.ForagePlayer;

import io.netty.buffer.ByteBuf;
import net.minecraft.block.Block;
import net.minecraft.init.Blocks;
import net.minecraftforge.fml.common.network.ByteBufUtils;
import net.minecraftforge.fml.common.network.simpleimpl.IMessage;

public class SetPlayerDisplayNames implements IMessage {

	public String player_names;
	public String player_displaynames;
	
	public SetPlayerDisplayNames() {	} //Default constructor is requried
	
	public SetPlayerDisplayNames(List<ForagePlayer> players, String[] names) {
		List<String> pnames = players.stream().map(p->p.get_player().getName()).collect(Collectors.toList());
		player_names = serializeArray(pnames.toArray(new String[0]));
		player_displaynames = serializeArray(names);
	}
	
	@Override
	public void fromBytes(ByteBuf buf) {
		player_names = ByteBufUtils.readUTF8String(buf);
		player_displaynames = ByteBufUtils.readUTF8String(buf);
	}

	@Override
	public void toBytes(ByteBuf buf) {
		ByteBufUtils.writeUTF8String(buf, player_names);
		ByteBufUtils.writeUTF8String(buf, player_displaynames);
	}
	
	//Helper functions to serialize/de-serialize arrays. See: https://stackoverflow.com/questions/43166009/correctly-convert-string-array-to-string-and-back-in-java
	public static String serializeArray(final String[] data) {
	    try (final ByteArrayOutputStream boas = new ByteArrayOutputStream();
	         final ObjectOutputStream oos = new ObjectOutputStream(boas)) {
	        oos.writeObject(data);
	        return Base64.getEncoder().encodeToString(boas.toByteArray());
	    } catch (IOException e) {
	        throw new RuntimeException(e);
	    }
	}
	
	public static String[] deserializeArray(final String data) {
	    try (final ByteArrayInputStream bias = new ByteArrayInputStream(Base64.getDecoder().decode(data));
	         final ObjectInputStream ois = new ObjectInputStream(bias)) {
	        return (String[]) ois.readObject();
	    } catch (IOException | ClassNotFoundException e) {
	        throw new RuntimeException(e);
	    }
	}

}
